module FsiX.AppStateActor

open System.IO
open System.Threading
open System.Threading.Tasks

open FSharpPlus
open PrettyPrompt
open FSharp.Compiler.Interactive.Shell

open FsiX.Features
open FsiX.AppState
open FsiX.ProjectLoading

let buildPipeline (middleware : Middleware list) evalFn =
  List.fold (fun next m -> m next) evalFn middleware
let evalFn token = 
  fun ({Code = code; }, st) ->
    try 
      st.OutStream.StartRecording()
      st.Session.EvalInteraction(code, token)
      let res = st.OutStream.StopRecording()
      {Result = Ok res; Metadata = Map.empty}, {st with EvalHistory = code :: st.EvalHistory}
    with e ->
      {Result = Error (e, st.OutStream.StopRecording()); Metadata = Map.empty}, st
let mkAppStateActor useAsp sln = MailboxProcessor.Start(fun mailbox ->
  let rec loop st middleware = async {
    let! cmd = mailbox.Receive()
    match cmd with 
    | Autocomplete (text, caret, word, reply) -> 
      let res = AutoCompletion.getCompletions st.Session text caret word
      reply.Reply res
      return! loop st middleware
    | SyntaxHighlight (text, reply) ->
      let res = SyntaxHighlighting.getFormatSpans text
      reply.Reply res
      return! loop st middleware
    | GetConfiguration reply -> 
      let promptConfigurationValue =
          st.Session.GetBoundValues()
          |> List.find (fun x -> x.Value.ReflectionType.Name = nameof PromptConfiguration)
          |> (fun v -> v.Value.ReflectionValue :?> PromptConfiguration)
      reply.Reply promptConfigurationValue

      return! loop st middleware
    | Eval (request, token, reply) -> 
      let pipeline = buildPipeline middleware (evalFn token)
      let res, newSt =
        try pipeline (request, st)
        with e -> {Result = Error (e, st.OutStream.StopRecording()); Metadata = Map.empty}, st
      reply.Reply res
      return! loop newSt middleware
    | Restore path ->
      match SaveRestore.restore st path with
      | Some newState -> return! loop newState middleware
      | None ->
        st.OutStream.WriteLine("Restoration failed")
        return! loop st middleware
    | AddMiddleware additionalMiddleware -> 
      return! loop st (middleware @ additionalMiddleware)
  }
  and init () = async {
    printfn "Welcome to FsiX!"
    printfn "Loading these projects: "
    for project in sln.Projects do
      printfn "%s" project.ProjectFileName
    let globalConfigTask = Configuration.loadGlobalConfig ()
    let localConfigTasks = sln.StartupFiles |> Seq.map File.ReadAllTextAsync |> Task.WhenAll
    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let args = solutionToFsiArgs useAsp sln


    let out = new BufferedStdoutWriter()
    let fsiSession =
        FsiEvaluationSession.Create(
            fsiConfig,
            args,
            new StreamReader(Stream.Null),
            out,
            stdout,
            collectible = true
        )
    let! globalConfig, localConfigs = 
      Task.map2 tuple2 globalConfigTask localConfigTasks |> Async.AwaitTask

    printfn "Loading configuration..."
    fsiSession.EvalInteraction(globalConfig, CancellationToken.None)

    for cfg in localConfigs do
      fsiSession.EvalInteraction(cfg, CancellationToken.None)
    out.Enable()

    let st = {Solution = sln;
              Session = fsiSession; 
              GlobalConfig = globalConfig;
              LocalConfigs = localConfigs;
              OutStream = out
              EvalHistory = []
              Custom = Map.empty}

    return! loop st []
  }
  init ()

)
