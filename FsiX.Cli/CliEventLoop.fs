module FsiX.Cli.CliEventLoop

open FsiX.AppState
open FsiX
open System.Threading

open FsiX.Cli.Logging
open FsiX.Cli.PrettyPromptCallbacks
open FsiX.Features


let startActor useAsp args = ActorCreation.mkCommonActorArgs cliLogger useAsp args |> ActorCreation.createActor

let runSimpleEval (actor: AppActor) code ct = task {
  let request = {EvalRequest.Code = code; Args = Map.empty}
  let! response = actor.PostAndAsyncReply(fun r -> Command.Eval(request, ct, r))
  return response
}
let loadConfiguration actor = task {
  cliLogger.LogInfo "Loading configuration..."
  let! config = Configuration.loadGlobalConfig ()
  let! {EvaluationResult = r} = runSimpleEval actor config CancellationToken.None
  match r with 
  | Error ex -> cliLogger.LogWarning <| ex.ToString()
  | Ok _ -> ()

  let! promptConfig = actor.PostAndAsyncReply(fun r -> Command.GetBoundValue("promptConfig", r))
  match promptConfig with 
  | Some x when (x :? PrettyPrompt.PromptConfiguration) ->
    cliLogger.LogDebug "Done!"
    actor.Post Command.EnableStdout
    return x :?> PrettyPrompt.PromptConfiguration
  | _ ->
    cliLogger.LogError <| "Cannot find prompt configuration!"
    System.Environment.Exit 1
    return failwith "cannot happen"
}
let runCliEventLoop useAsp args () = task {
  let! appActor = startActor useAsp args
  let! config = loadConfiguration appActor 

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
            let! response = runSimpleEval appActor userLine.Text userLine.CancellationToken
            for d in response.Diagnostics do
                match d.Severity with
                | Diagnostics.Info -> cliLogger.LogInfo d.Message
                | Diagnostics.Hidden -> cliLogger.LogDebug d.Message
                | Diagnostics.Warning -> cliLogger.LogWarning d.Message
                | Diagnostics.Error -> cliLogger.LogError d.Message
            match response.EvaluationResult with 
            | Ok _ -> () //do nothing as TextWriterRecorder will print colored result
            | Error e -> cliLogger.LogError <| e.ToString()

      with _ -> ()

}

