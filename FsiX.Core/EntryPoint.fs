module FsiX.EntryPoint

open System.Collections.Generic
open System.IO

open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization

open FsiX.Features
open FsiX.Middleware
open FsiX.ProjectLoading
open FsiX.AppState
open PrettyPrompt.Completion
open PrettyPrompt.Highlighting
open PrettyPrompt.Documents

#nowarn "0057"
type private FSharpToken with
    member this.IsControlKeyword =
        match this.Kind with
        | FSharpTokenKind.OffsideDo
        | FSharpTokenKind.OffsideElse
        | FSharpTokenKind.OffsideThen
        | FSharpTokenKind.Match
        | FSharpTokenKind.MatchBang
        | FSharpTokenKind.If
        | FSharpTokenKind.Then
        | FSharpTokenKind.Elif
        | FSharpTokenKind.Else
        | FSharpTokenKind.Assert
        | FSharpTokenKind.While
        | FSharpTokenKind.WhileBang
        | FSharpTokenKind.Do
        | FSharpTokenKind.DoBang
        | FSharpTokenKind.For
        | FSharpTokenKind.To
        | FSharpTokenKind.DownTo
        | FSharpTokenKind.Try
        | FSharpTokenKind.Yield
        | FSharpTokenKind.YieldBang -> true
        | _ -> false

type FsiCallBacks(app: MailboxProcessor<AppState.Command>) =
    inherit PrettyPrompt.PromptCallbacks()

    override _.ShouldOpenCompletionWindowAsync (text: string, caret: int, keyPress: PrettyPrompt.Consoles.KeyPress, cancellationToken: System.Threading.CancellationToken): System.Threading.Tasks.Task<bool> = task { return true }
    override _.GetCompletionItemsAsync(text, caret, spanToBeReplaced, _) =
        task {
            let typedWord = text.Substring(spanToBeReplaced.Start, spanToBeReplaced.Length)
            return 
              app.PostAndReply(fun r -> Autocomplete(text, caret, typedWord, r))
              :> IReadOnlyList<CompletionItem>
        }

    override _.HighlightCallbackAsync (text: string, cancellationToken: System.Threading.CancellationToken) =
        let lineLengths = text.Split('\n') |> Seq.map (String.length >> (+) 1) |> Seq.toArray
        let posToIndex (p: pos) = (lineLengths |> Seq.take (p.Line - 1) |> Seq.sum) +  p.Column
        let rangeToSpan (r: range) =
            let start = posToIndex r.Start
            TextSpan(start, posToIndex r.End - start)
        let toFormatSpan (token: FSharpToken) =
            let mkSpan (color: AnsiColor) =
                FormatSpan(rangeToSpan token.Range, ConsoleFormat(System.Nullable color))
                |> Some
            if token.IsIdentifier then mkSpan AnsiColor.BrightBlue
            else if token.IsControlKeyword then mkSpan AnsiColor.BrightRed
            else if token.IsKeyword then mkSpan AnsiColor.Blue
            else if token.IsNumericLiteral then mkSpan AnsiColor.BrightGreen
            else if token.IsStringLiteral then mkSpan AnsiColor.Red
            else None
        task {
            let tokens = ResizeArray()
            FSharpLexer.Tokenize(
                SourceText.ofString text,
                (fun token -> toFormatSpan token |> Option.iter tokens.Add))
            return tokens
        }

let main useAsp args () =
    task {
        let parsedArgs = FsiX.Args.parser.ParseCommandLine(args).GetAllResults()
        let appActor =
            let sln = loadSolution parsedArgs
            AppState.mkAppStateActor useAsp sln
        let middleware = [
          Directives.viBindMiddleware
          Directives.OpenDirective.openDirectiveMiddleware
          ComputationExpression.compExprMiddleware
        ]
        appActor.Post(AddMiddleware middleware)

        let config = appActor.PostAndReply GetConfiguration

        let prompt =
            PrettyPrompt.Prompt(
                persistentHistoryFilepath = "./.fsix_history",
                callbacks = FsiCallBacks appActor,
                configuration = config
            )
        while true do
            try
                let! userLine = prompt.ReadLineAsync()
                if userLine.IsSuccess then
                  let request = {Code = userLine.Text; Args = Map ["reload", "" ]} 
                  let response = appActor.PostAndReply(fun r -> Command.Eval(request, userLine.CancellationToken, r))
                  for m in response.Metadata.Values do
                    Utils.Logging.logInfo m
            with _ -> ()

    }

