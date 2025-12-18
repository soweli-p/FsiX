module FsiX.Cli.CliEventLoop

open FsiX.AppState
open FsiX
open System.Threading

open FsiX.Cli.Logging
open FsiX.Cli.PrettyPromptCallbacks
open FsiX.Features



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


module Daemon =
  open System.Net.Sockets
  open System.Net

  open FsiX.Daemon
  open FSharpPlus

  let createActorWithDaemon actorArgs = ActorCreation.createActor {actorArgs with Middleware = actorArgs.Middleware @ [ActorInit.captureStdioMiddleware]}

  let startTcp actor (addr: string) port = 
    let addr = IPAddress.Parse addr
    let listener = new TcpListener(addr, port)
    Rpc.Tcp.mkJsonRpc listener |> Task.map (fun jsonRpc -> 
      Rpc.Procedures.addProcedures actor jsonRpc
      jsonRpc.StartListening()
    )


let runCliEventLoop useAsp args () = task {
  let args = Args.parser.ParseCommandLine args

  let actorArgs = ActorCreation.mkCommonActorArgs cliLogger useAsp (args.GetAllResults())
  let! appActor = 
    match args.TryGetResult Args.Arguments.Daemon with
    | None -> ActorCreation.createActor actorArgs
    | Some _ -> Daemon.createActorWithDaemon actorArgs

  let! config = loadConfiguration appActor 

  match args.TryGetResult Args.Arguments.Daemon with
  | Some (addr, port) -> Daemon.startTcp appActor addr port |> ignore
  | None -> ()


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

