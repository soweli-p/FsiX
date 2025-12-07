module FsiX.ActorCreation

open FsiX.Middleware
open FsiX.ProjectLoading
open FsiX.AppState

let commonMiddleware: AppState.Middleware list = [
    Directives.viBindMiddleware
    Directives.OpenDirective.openDirectiveMiddleware
    ComputationExpression.compExprMiddleware
    HotReloading.hotReloadingMiddleware
]


let commonInitFunctions = [HotReloading.hotReloadingInitFunction]

open System.IO

type ActorArgs = {
  Middleware: AppState.Middleware list
  InitFunctions: (Solution -> string * objnull) list
  Logger: Utils.ILogger
  OutStream: TextWriter
  UseAsp: bool
  Args: string[]

}

let createActor a = task {
  let parsedArgs = FsiX.Args.parser.ParseCommandLine(a.Args).GetAllResults()
  let sln = loadSolution a.Logger parsedArgs
  let customData = 
    a.InitFunctions
    |> Seq.map (fun fn -> fn sln)
    |> Map.ofSeq
  let appActor = mkAppStateActor a.Logger customData a.OutStream a.UseAsp sln
  do! appActor.PostAndAsyncReply(fun r -> AddMiddleware (a.Middleware, r))
  return appActor
}

let mkCommonActorArgs logger useAsp args = 
  {
    Middleware = commonMiddleware
    InitFunctions = commonInitFunctions
    UseAsp = useAsp
    Args = args
    OutStream = stdout
    Logger = logger
  } 
