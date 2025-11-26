open System
open StreamJsonRpc
open Nerdbank.Streams


open FsiX.Daemon

open System.Text.Json
open System.Text.Json.Serialization
let mkJsonFormatter () =
  let jsOptions =
    JsonFSharpOptions.Default()
    |> _.WithUnionUnwrapFieldlessTags()
    |> _.WithUnionNamedFields()
    |> _.WithOverrides(fun o -> dict [
            typedefof<Result<_, _>>, o
                .WithUnionNamedFields()
                .WithUnionTagName("case")
                .WithUnionInternalTag()
                .WithOverrideMembers(dict [
                    nameof Ok, [
                        JsonNameAttribute "ok"
                        JsonNameAttribute("data", Field = "ResultValue")
                    ]
                    nameof Error, [
                        JsonNameAttribute "error"
                        JsonNameAttribute("error", Field = "ErrorValue")
                    ]
                ])
    ])
    |> _.ToJsonSerializerOptions()
  jsOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
  let formatter = new SystemTextJsonFormatter()
  formatter.JsonSerializerOptions <- jsOptions

  formatter

let mkStdioJsonRpc () =
  let stream = FullDuplexStream.Splice(Console.OpenStandardInput(), Console.OpenStandardOutput())
  let handler = new HeaderDelimitedMessageHandler(stream, mkJsonFormatter ())
  new JsonRpc(handler)

[<EntryPoint>]
let main args =
  let rpc = mkStdioJsonRpc ()
  Rpc.startAndInitRpc args rpc
  rpc.Completion.GetAwaiter().GetResult()
  Console.ReadLine()
  0
