module FsiX.AppState

open System
open System.IO

open System.Threading
open System.Threading.Tasks
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Interactive.Shell
open FSharpPlus
open Fantomas.FCS.Diagnostics
open FsiX.Features
open FsiX.ProjectLoading
open FsiX.Utils

open PrettyPrompt


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
  Logger: ILogger
  GlobalConfig: string
  LocalConfigs: string array
  Session: FsiEvaluationSession
  OutStream: BufferedStdoutWriter
  Custom: Map<string, obj>
  }


type Diagnostic = {
  Message: string
  Subcategory: string
  Severity: FSharpDiagnosticSeverity } with
  static member mkDiagnostic (fsDiagnostic: FSharpDiagnostic) =
    let mapSeverity = function
      | FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Error -> FSharpDiagnosticSeverity.Error
      | FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Hidden -> FSharpDiagnosticSeverity.Hidden
      | FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Info -> FSharpDiagnosticSeverity.Info
      | FSharp.Compiler.Diagnostics.FSharpDiagnosticSeverity.Warning -> FSharpDiagnosticSeverity.Warning
    {Message = fsDiagnostic.Message; Severity = mapSeverity fsDiagnostic.Severity; Subcategory = fsDiagnostic.Subcategory}
type EvalResponse = {
  ResultOutput: string
  Error: Exception option
  Diagnostics: Diagnostic array
  EvaluatedCode: string
  Metadata: Map<string, obj>
  }
type EvalRequest = {Code: string; Args: Map<string, obj>; }

type MiddlewareNext = EvalRequest * AppState -> EvalResponse * AppState
type Middleware = MiddlewareNext -> EvalRequest * AppState -> EvalResponse * AppState

type Command = 
  | Eval of EvalRequest * CancellationToken * AsyncReplyChannel<EvalResponse>
  | Autocomplete of text: string * caret: int * word: string * AsyncReplyChannel<list<Completion.CompletionItem>>
  | GetConfiguration of AsyncReplyChannel<PromptConfiguration>
  | AddMiddleware of Middleware list


let buildPipeline (middleware : Middleware list) evalFn =
  List.fold (fun next m -> m next) evalFn middleware
let evalFn token = 
  fun ({Code = code; }, st) ->
    st.OutStream.StartRecording()
    let evalRes, diagnostics = st.Session.EvalInteractionNonThrowing(code, token)
    let error =
      match evalRes with
      | Choice1Of2 _ -> None
      | Choice2Of2 ex -> Some ex
    let resText = st.OutStream.StopRecording()
    let diagnostics = diagnostics |> Array.map Diagnostic.mkDiagnostic
    {ResultOutput = resText; Error = error; Diagnostics = diagnostics; Metadata = Map.empty; EvaluatedCode = code}, st
let mkAppStateActor logger useAsp sln = MailboxProcessor.Start(fun mailbox ->
  let rec loop st middleware = async {
    let! cmd = mailbox.Receive()
    match cmd with 
    | Autocomplete (text, caret, word, reply) -> 
      let res = AutoCompletion.getCompletions st.Session text caret word
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
              LocalConfigs = localConfigs
              Logger = logger
              OutStream = out
              Custom = Map.empty}

    return! loop st []
  }
  init ()

)
