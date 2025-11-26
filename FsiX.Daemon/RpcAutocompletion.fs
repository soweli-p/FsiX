module FsiX.Daemon.RpcAutocompletion

open FsiX
open FSharpPlus

type CompletionItem = {
  DisplayText: string
  ReplacementText: string
  Kind: string
  Description: string option
}

let mapToRpcCompletion (i: FsiX.Features.AutoCompletion.CompletionItem) =
  let description = 
    i.GetDescription
    |> Option.map (fun fn -> fn () |> Seq.map _.Text |> String.concat "")
  {
    DisplayText = i.DisplayText
    ReplacementText = i.ReplacementText
    Kind = i.Kind
    Description = description
  }

let getCompletions (actor: AppState.AppActor) text caret word = 
  actor.PostAndAsyncReply(fun r -> AppState.Command.Autocomplete(text, caret, word, r))
  |>> List.map mapToRpcCompletion
  |> Async.StartAsTask
