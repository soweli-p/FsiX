module FsiX.Daemon.ActorInit

open FsiX
open System.IO
open System.Threading


module Configuration = 
  open FsiX.Utils.Configuration
  let loadConfig () = task {
    let dirPath = getConfigDir ()
    let configPath = Path.Combine(dirPath, "daemon.fsx")
    if File.Exists configPath then
      return! File.ReadAllTextAsync configPath
    else
      let! defaultConfig = getBaseConfigString ()
      do! File.WriteAllTextAsync(configPath, defaultConfig)
      return defaultConfig
  }


open System
let captureStdioMiddleware next (request: AppState.EvalRequest, st) =
  let origOut = stdout
  let origErr = stderr
  let origIn = stdin
  let sout = new StringWriter()
  let serr = new StringWriter()
  Console.SetOut sout
  Console.SetError serr
  Console.SetIn StringReader.Null //todo add ability to provide input from vscode
  let (response: AppState.EvalResponse), st = next (request, st)
  Console.SetIn origIn
  Console.SetOut origOut
  Console.SetError origErr
  let newMetadata =
    response.Metadata
    |> Map.add "stdout" (sout.ToString())
    |> Map.add "stderr" (serr.ToString())
  {response with Metadata = newMetadata}, st



let startAndInitActor logger args = task {
  let actorArgs = ActorCreation.mkCommonActorArgs logger true args
  let actorArgs =
    { actorArgs with Middleware = ActorCreation.commonMiddleware @ [ captureStdioMiddleware ];
                     OutStream = TextWriter.Null }
  let! appActor = ActorCreation.createActor actorArgs

  let! config = Configuration.loadConfig ()
  let request = { AppState.EvalRequest.Code = config; AppState.EvalRequest.Args = Map.empty }
  let! response = appActor.PostAndAsyncReply(fun r -> AppState.Eval (request, CancellationToken.None, r))

  match response.EvaluationResult with 
  | Ok _ -> return Ok appActor
  | Error e -> return Error e
  
}
