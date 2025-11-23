module FsiX.Cli.PrettyPromptCallbacks

open FSharpPlus
open FsiX.AppState

open FSharp.Compiler.Text

open PrettyPrompt.Completion
open PrettyPrompt.Highlighting
open System.Threading.Tasks

open System.Collections.Generic

module AutoCompletionMapping =
  let tagToColor =
      function
      | TextTag.Keyword -> AnsiColor.Blue
      | TextTag.Function -> AnsiColor.Cyan
      | _ -> AnsiColor.White

  let mkSpan (builder: FormattedStringBuilder) (tag: TaggedText) =
      builder.Append(tag.Text, FormatSpan(0, tag.Text.Length, tagToColor tag.Tag))
  let mapCompletionItem (i: FsiX.Features.AutoCompletion.CompletionItem) = 
    match i.GetDescription with 
    | None -> CompletionItem(replacementText=i.ReplacementText, displayText=i.DisplayText)
    | Some fn -> CompletionItem(replacementText=i.ReplacementText, displayText=i.DisplayText,
        getExtendedDescription=(
          konst () 
          >> fn
          >> Seq.fold mkSpan (FormattedStringBuilder())
          >> _.ToFormattedString()
          >> Task.FromResult
        )
      )



type FsiCallBacks(app: AppActor) =
    inherit PrettyPrompt.PromptCallbacks()

    override _.ShouldOpenCompletionWindowAsync (text: string, caret: int, keyPress: PrettyPrompt.Consoles.KeyPress, cancellationToken: System.Threading.CancellationToken): System.Threading.Tasks.Task<bool> = task { return true }
    override _.GetCompletionItemsAsync(text, caret, spanToBeReplaced, _) =
        task {
            let typedWord = text.Substring(spanToBeReplaced.Start, spanToBeReplaced.Length)
            let! items = app.PostAndAsyncReply(fun r -> Autocomplete(text, caret, typedWord, r)) |> Async.StartAsTask
            return
              items
              |> List.map AutoCompletionMapping.mapCompletionItem
              :> IReadOnlyList<CompletionItem>
        }
