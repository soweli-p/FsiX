module FsiX.Daemon.ActorInit

open FsiX.ProjectLoading
open FsiX.Middleware
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
let captureStdoutMiddleware next (request: AppState.EvalRequest, st) =
  let origOut = stdout
  let origErr = stderr
  let sout = new StringWriter()
  Console.SetOut sout
  Console.SetError sout
  let (response: AppState.EvalResponse), st = next (request, st)
  Console.SetOut origOut
  Console.SetError origErr
  let newMetadata =
    response.Metadata
    |> Map.add "stdout" (sout.ToString())
  {response with Metadata = newMetadata}, st

let startAndInitActor logger args = task {
  let parsedArgs = FsiX.Args.parser.ParseCommandLine(args).GetAllResults()
  let appActor =
    let sln = loadSolution logger parsedArgs
    AppState.mkAppStateActor logger TextWriter.Null true sln
  let middleware = [
    Directives.viBindMiddleware
    Directives.OpenDirective.openDirectiveMiddleware
    ComputationExpression.compExprMiddleware
    HotReloading.hotReloadingMiddleware
    captureStdoutMiddleware
  ]
  do! appActor.PostAndAsyncReply(fun r -> AppState.AddMiddleware (middleware, r))

  let! config = Configuration.loadConfig ()
  let request = { AppState.EvalRequest.Code = config; AppState.EvalRequest.Args = Map.empty }
  let! response = appActor.PostAndAsyncReply(fun r -> AppState.Eval (request, CancellationToken.None, r))

  match response.EvaluationResult with 
  | Ok _ -> return Ok appActor
  | Error e -> return Error e
  
}
