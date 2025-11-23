module FsiX.Cli.PrettyPromptCallbacks

open FSharpPlus
open FsiX.AppState

open PrettyPrompt.Completion
open System.Threading.Tasks

open System.Collections.Generic

type FsiCallBacks(app: AppActor) =
    inherit PrettyPrompt.PromptCallbacks()

    override _.ShouldOpenCompletionWindowAsync (text: string, caret: int, keyPress: PrettyPrompt.Consoles.KeyPress, cancellationToken: System.Threading.CancellationToken): System.Threading.Tasks.Task<bool> = task { return true }
    override _.GetCompletionItemsAsync(text, caret, spanToBeReplaced, _) =
        task {
            let typedWord = text.Substring(spanToBeReplaced.Start, spanToBeReplaced.Length)
            let! items = app.PostAndAsyncReply(fun r -> Autocomplete(text, caret, typedWord, r)) |> Async.StartAsTask
            return
              items
              |> List.map (fun i ->
                CompletionItem(replacementText=i.ReplacementText, displayText=i.DisplayText, 
                  getExtendedDescription=(
                    konst () 
                    >> i.GetFormattedDescription
                    >> Option.defaultValue (new PrettyPrompt.Highlighting.FormattedString()) 
                    >> Task.FromResult)
                )
              )
              :> IReadOnlyList<CompletionItem>
        }
