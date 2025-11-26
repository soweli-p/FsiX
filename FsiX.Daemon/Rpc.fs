module FsiX.Daemon.Rpc

open System.Threading
open System.Threading.Tasks

open FsiX.AppState

open StreamJsonRpc
open ActorInit
module Procedures = 
  type EvalDelegate = delegate of code: string * args: Map<string, obj> * CancellationToken -> Task<FsiX.AppState.EvalResponse>
  type CompletionDelegate = delegate of text: string * caret: int * word: string -> Task<RpcAutocompletion.CompletionItem list>
  let addProcedures (appActor: AppActor) (rpc: JsonRpc) =
    rpc.AddLocalRpcMethod("eval", 
      new EvalDelegate(fun code args ct -> appActor.PostAndAsyncReply(fun r -> Eval({Code = code; Args = args}, ct, r)) |> Async.StartAsTask))
    rpc.AddLocalRpcMethod("autocomplete", 
      new CompletionDelegate(RpcAutocompletion.getCompletions appActor))

module Events = 
  let logError (rpc: JsonRpc) message = rpc.NotifyWithParameterObjectAsync("logging", {|Level = "error"; Message = message|})
  let logInfo (rpc: JsonRpc) message = rpc.NotifyWithParameterObjectAsync("logging", {|Level = "info"; Message = message|})
  let logDebug (rpc: JsonRpc) message = rpc.NotifyWithParameterObjectAsync("logging", {|Level = "debug"; Message = message|})
  let logWarning (rpc: JsonRpc) message = rpc.NotifyWithParameterObjectAsync("logging", {|Level = "warning"; Message = message|})

  open FsiX.Utils
  type RpcLogger(rpc: JsonRpc) = 
    interface ILogger with
        member _.LogDebug s = logDebug rpc s |> ignore
        member _.LogInfo s = logInfo rpc s |> ignore
        member _.LogError s = logError rpc s |> ignore
        member _.LogWarning s = logWarning rpc s |> ignore

  let initDone (rpc: JsonRpc) (initResult: Result<unit, System.Exception>) = 
    rpc.NotifyWithParameterObjectAsync("initialized", initResult)


open Events
open Procedures
let startAndInitRpc args (rpc: JsonRpc) =
  let logger = new RpcLogger(rpc)
  let actor = startAndInitActor logger args |> _.GetAwaiter() |> _.GetResult()
  match actor with 
  | Ok actor ->
    addProcedures actor rpc
    rpc.StartListening()
    initDone rpc (Ok ()) |> ignore
  | Result.Error ex -> initDone rpc (Result.Error ex) |> ignore

