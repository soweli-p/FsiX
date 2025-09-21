﻿module FsiX.EntryPoint

open System.Collections.Generic
open System.IO

open FsiX.Features
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

let main useAsp args () =
    task {
        let parsedArgs = FsiX.Args.parser.ParseCommandLine(args).GetAllResults()
        let appActor =
            let sln = loadSolution parsedArgs
            AppState.mkAppStateActor useAsp sln
        let middleware = [
          DirectiveProcessor.viBindMiddleware
          DirectiveProcessor.OpenDirective.openDirectiveMiddleware
          ComputationExpressionMiddleware.compExprMiddleware
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

