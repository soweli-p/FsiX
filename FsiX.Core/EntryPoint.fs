module FsiX.EntryPoint

open System.Collections.Generic

open FsiX.Middleware
open FsiX.ProjectLoading
open FsiX.AppState
open PrettyPrompt.Completion

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
        app.PostAndAsyncReply(fun r -> SyntaxHighlight(text, r))
        |> Async.StartAsTask

let main useAsp args () =
    task {
        let parsedArgs = FsiX.Args.parser.ParseCommandLine(args).GetAllResults()
        let appActor =
            let sln = loadSolution parsedArgs
            AppState.mkAppStateActor useAsp sln
        let middleware = [
          Directives.HelpDirective.helpDirectiveMiddleware
          Directives.OpenDirective.openDirectiveMiddleware
          Directives.HelpDirective.htypeDirectiveMiddleware
          Directives.SaveRestoreDirectives.saveDirectiveMiddleware
          Directives.SaveRestoreDirectives.restoreDirectiveMiddleware
          Directives.viBindMiddleware
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
                  match response.Result with
                  | Error (:? FSharp.Compiler.Interactive.Shell.FsiCompilationException, _) -> ()
                  | Error (e, s) -> printfn "%A\n%s" e s
                  | Ok _ -> ()
            with ex -> printfn "%A" ex

    }

