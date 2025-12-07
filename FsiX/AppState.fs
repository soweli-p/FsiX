module FsiX.AppState

open System
open System.IO

open System.Threading
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Interactive.Shell
open FSharpPlus
open FsiX.Features
open FsiX.ProjectLoading
open FsiX.Utils



type FilePath = string


open System.Text
type TextWriterRecorder(writerToRecord: TextWriter) =
  inherit TextWriter()

  let mutable isEnabled = false
  let mutable recording: StringBuilder option = None

  override _.Encoding = writerToRecord.Encoding

  override _.Write(value: char) =
    match recording with 
    | None -> ()
    | Some recorder -> 
      recorder.Append value |> ignore

    if isEnabled then writerToRecord.Write value

  override _.Write(value: string) =
    match recording with 
    | None -> ()
    | Some recorder -> 
      recorder.Append value |> ignore
    if isEnabled then writerToRecord.Write value

  override _.Write(bufferArr: char[], index: int, count: int) =
    match recording with 
    | None -> ()
    | Some recorder -> 
      recorder.Append(bufferArr, index, count) |> ignore
    if isEnabled then writerToRecord.Write(bufferArr, index, count)

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
      writerToRecord.Flush()


type AppState = {
  Solution: Solution
  Logger: ILogger
  Session: FsiEvaluationSession
  OutStream: TextWriterRecorder
  Custom: Map<string, obj>
  }



type EvalResponse = {
  EvaluationResult: Result<string, Exception>
  Diagnostics: Diagnostics.Diagnostic array
  EvaluatedCode: string
  Metadata: Map<string, objnull>
  }
type EvalRequest = {Code: string; Args: Map<string, obj>; }

type MiddlewareNext = EvalRequest * AppState -> EvalResponse * AppState
type Middleware = MiddlewareNext -> EvalRequest * AppState -> EvalResponse * AppState

type Command = 
  | Eval of EvalRequest * CancellationToken * AsyncReplyChannel<EvalResponse>
  | Autocomplete of text: string * caret: int * word: string * AsyncReplyChannel<list<AutoCompletion.CompletionItem>>
  | GetBoundValue of name: string * AsyncReplyChannel<obj Option>
  | AddMiddleware of Middleware list * AsyncReplyChannel<unit>
  | GetDiagnostics of text: string * AsyncReplyChannel<Diagnostics.Diagnostic array>
  | EnableStdout


type AppActor = MailboxProcessor<Command>

let wrapErrorMiddleware next (request, st) =
  try 
    next (request, st)
  with e -> 
    let errResponse = {
      EvaluationResult = Error <| new Exception("FsiXInternal error occured", e)
      Diagnostics = [||]
      EvaluatedCode = ""
      Metadata = Map.empty
    }
    errResponse, st


//fold - first m in list would be the closest to eval
//foldBack - last m in list would be the closest to eval
//better to use foldBack as we can simply push new m's and it's more intuitive that 
//the last m would evaluate the latest
let buildPipeline (middleware : Middleware list) evalFn =
  List.foldBack (fun m next -> m next) middleware evalFn
let evalFn (token: CancellationToken) = 
  fun ({Code = code; }, st) ->
    st.OutStream.StartRecording()
    let thread = Thread.CurrentThread
    token.Register(fun () -> thread.Interrupt()) |> ignore
    let evalRes, diagnostics = st.Session.EvalInteractionNonThrowing(code, token)
    let diagnostics = diagnostics |> Array.map Diagnostics.Diagnostic.mkDiagnostic
    let evalRes = 
      match evalRes with
      | Choice1Of2 _ -> Ok <| st.OutStream.StopRecording()
      | Choice2Of2 ex -> Error <| ex
    st.OutStream.StopRecording() |> ignore
    {EvaluationResult = evalRes; Diagnostics = diagnostics; Metadata = Map.empty; EvaluatedCode = code}, st



open System.Threading.Tasks
open System.Threading
let mkAppStateActor (logger: ILogger) (initCustomData: Map<string, obj>) outStream useAsp sln = 
  MailboxProcessor.Start(fun mailbox ->
  let rec loop st middleware = async {
    let! cmd = mailbox.Receive()
    match cmd with 
    | Autocomplete (text, caret, word, reply) -> 
      let res = AutoCompletion.getCompletions st.Session text caret word
      reply.Reply res
      return! loop st middleware
    | GetDiagnostics(text, reply) ->
      let res = Diagnostics.getDiagnostics st.Session text
      reply.Reply res
      return! loop st middleware
    | EnableStdout -> 
      st.OutStream.Enable()
      return! loop st middleware
    | GetBoundValue (name, reply) -> 
      st.Session.GetBoundValues() 
      |> List.tryFind (fun x -> x.Name = name)
      |>> (fun v -> v.Value.ReflectionValue)
      >>= Option.ofObj
      |> reply.Reply

      return! loop st middleware
    | Eval (request, token, reply) -> 
      let pipeline = buildPipeline (wrapErrorMiddleware :: middleware) (evalFn token)
      let res, newSt = pipeline (request, st)
      reply.Reply res
      return! loop newSt middleware
    | AddMiddleware (additionalMiddleware, r) -> 
      r.Reply(()) // needed to know when middleware was added so actor is initialized
      return! loop st (additionalMiddleware @ middleware)
  }
  and init () = async {
    logger.LogInfo "Welcome to FsiX!"
    logger.LogInfo "Loading these projects: "
    for project in sln.Projects do
      logger.LogInfo project.ProjectFileName

    let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
    let args = solutionToFsiArgs logger useAsp sln


    let recorder = new TextWriterRecorder(outStream)
    let fsiSession =
        FsiEvaluationSession.Create(
            fsiConfig,
            args,
            new StreamReader(Stream.Null),
            recorder,
            outStream,
            collectible = true
        )
    for fileName in sln.StartupFiles do
      logger.LogInfo $"Loading {fileName}"
      let! fileContents = File.ReadAllTextAsync fileName |> Async.AwaitTask
      fsiSession.EvalInteraction(fileContents, CancellationToken.None)

    let st = {Solution = sln;
              Session = fsiSession; 
              Logger = logger
              OutStream = recorder
              Custom = initCustomData}

    return! loop st []
  }
  init ()

)
