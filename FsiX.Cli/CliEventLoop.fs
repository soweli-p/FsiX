module FsiX.Cli.CliEventLoop

open FsiX.Middleware
open FsiX.ProjectLoading
open FsiX.AppState
open FsiX
open System.Threading

open FsiX.Cli.Logging
open FsiX.Cli.PrettyPromptCallbacks


let startActor useAsp args = task {
  let parsedArgs = FsiX.Args.parser.ParseCommandLine(args).GetAllResults()
  let appActor =
    let sln = loadSolution cliLogger parsedArgs
    AppState.mkAppStateActor cliLogger stdout useAsp sln
  let middleware = [
    Directives.viBindMiddleware
    Directives.OpenDirective.openDirectiveMiddleware
    ComputationExpression.compExprMiddleware
    HotReloading.hotReloadingMiddleware
  ]
  do! appActor.PostAndAsyncReply(fun r -> AddMiddleware (middleware, r))
  return appActor
}
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
  | Result.Error ex -> cliLogger.LogWarning <| ex.ToString()
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
                | Info -> cliLogger.LogInfo d.Message
                | Hidden -> cliLogger.LogDebug d.Message
                | Warning -> cliLogger.LogWarning d.Message
                | Error -> cliLogger.LogError d.Message
            match response.EvaluationResult with 
            | Ok _ -> () //do nothing as TextWriterRecorder will print colored result
            | Result.Error e -> cliLogger.LogError <| e.ToString()

      with _ -> ()

}

