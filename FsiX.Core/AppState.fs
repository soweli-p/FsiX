module FsiX.AppState

open System
open System.IO
open System.Text
open System.Collections.Generic

open System.Threading
open FSharp.Compiler.Interactive.Shell
open FsiX.ProjectLoading

open PrettyPrompt
open PrettyPrompt.Highlighting

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
  EvalHistory: string list
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
  | Restore of string
  | AddMiddleware of Middleware list
