module FsiX.AppState

open System
open System.IO
open System.Collections.Generic

open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Interactive.Shell
open FSharpPlus
open FsiX.Features
open FsiX.ProjectLoading
open FsiX.Utils

open PrettyPrompt
open PrettyPrompt.Highlighting


type FilePath = string


open System.Text
type BufferedStdoutWriter() =
  inherit TextWriter()

  let realStdout = Console.Out
  let mutable isEnabled = false
  let mutable recording: StringBuilder option = None

  override _.Encoding = realStdout.Encoding

  override _.Write(value: char) =
    match recording with 
    | None -> ()
    | Some recorder -> 
      recorder.Append value |> ignore

    if isEnabled then realStdout.Write value

  override _.Write(value: string) =
    match recording with 
    | None -> ()
    | Some recorder -> 
      recorder.Append value |> ignore
    if isEnabled then realStdout.Write value

  override _.Write(bufferArr: char[], index: int, count: int) =
    match recording with 
    | None -> ()
    | Some recorder -> 
      recorder.Append(bufferArr, index, count) |> ignore
    if isEnabled then realStdout.Write(bufferArr, index, count)

  member _.Enable() =
    isEnabled <- true
  member _.StartRecording() =
    recording <- Some <| new StringBuilder ()
  member _.StopRecording() =
    match recording with
    | None -> ""
    | Some recorder ->
      recording <- None
      recorder.ToString()

  override _.Flush() =
      realStdout.Flush()


type AppState = {
  Solution: Solution
  GlobalConfig: string
  LocalConfigs: string array
  Session: FsiEvaluationSession
  OutStream: BufferedStdoutWriter
  Custom: Map<string, obj>
  }


type EvalResponse = {Result: Result<string, Exception * string>; Metadata: Map<string, string>}
type EvalRequest = {Code: string; Args: Map<string, string>; }

type MiddlewareNext = EvalRequest * AppState -> EvalResponse * AppState
type Middleware = MiddlewareNext -> EvalRequest * AppState -> EvalResponse * AppState

type Command = 
  | Eval of EvalRequest * CancellationToken * AsyncReplyChannel<EvalResponse>
  | Autocomplete of text: string * caret: int * word: string * AsyncReplyChannel<list<Completion.CompletionItem>>
  | SyntaxHighlight of text: string * AsyncReplyChannel<IReadOnlyCollection<FormatSpan>>
  | GetConfiguration of AsyncReplyChannel<PromptConfiguration>
  | AddMiddleware of Middleware list


let buildPipeline (middleware : Middleware list) evalFn =
  List.fold (fun next m -> m next) evalFn middleware
let evalFn token = 
  fun ({Code = code; }, st) ->
    try 
      st.OutStream.StartRecording()
      st.Session.EvalInteraction(code, token)
      let res = st.OutStream.StopRecording()
      {Result = Ok res; Metadata = Map.empty}, st
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
      let res, newSt = pipeline (request, st)
      reply.Reply res
      return! loop newSt middleware
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
              Custom = Map.empty}

    return! loop st []
  }
  init ()

)
