module FsiX.EntryPoint

open System.Collections.Generic
open System.IO

open Fantomas.FCS.Diagnostics
open FsiX.Features
open FsiX.Middleware
open FsiX.ProjectLoading
open FsiX.AppState
open FsiX.Utils
open PrettyPrompt.Completion

type CliLogger() =
    interface ILogger with
        member this.LogInfo s = Logging.logInfo s
        member this.LogError s = Logging.logError s
        member this.LogWarning s = Logging.logWarning s

//todo make FsiX.Cli, move this there
//and make FsiX.NRepl for another impl
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

//todo get args from config some flags like FsiXFlags.enableHotReload <- true
//will work only for cli
let cliDefaultArgsMiddleware next (request, st) =
    next (request, st)
let main useAsp args () =
    task {
        let parsedArgs = FsiX.Args.parser.ParseCommandLine(args).GetAllResults()
        let appActor =
            let sln = loadSolution parsedArgs
            AppState.mkAppStateActor (CliLogger()) useAsp sln
        let middleware = [
          cliDefaultArgsMiddleware
          Directives.viBindMiddleware
          Directives.OpenDirective.openDirectiveMiddleware
          ComputationExpression.compExprMiddleware
          HotReloading.hotReloadingMiddleware
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
                  let request = {Code = userLine.Text; Args = Map ["hotReload", true ]} 
                  let response = appActor.PostAndReply(fun r -> Command.Eval(request, userLine.CancellationToken, r))
                  for d in response.Diagnostics do
                      match d.Severity with
                      | FSharpDiagnosticSeverity.Hidden | FSharpDiagnosticSeverity.Info -> Logging.logInfo d.Message
                      | FSharpDiagnosticSeverity.Warning -> Logging.logWarning d.Message
                      | FSharpDiagnosticSeverity.Error -> Logging.logError d.Message

            with _ -> ()

    }

