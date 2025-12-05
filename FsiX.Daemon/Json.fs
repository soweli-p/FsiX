module FsiX.Daemon.Json

open System.Text.Json
open System.Text.Json.Serialization

open StreamJsonRpc

type ObjectAsConcreteTypeConverter() =
    inherit JsonConverter<obj>()

    override _.Read(reader, _, _) =
        match reader.TokenType with
        | JsonTokenType.True -> box true
        | JsonTokenType.False -> box false
        | JsonTokenType.Number ->
            if reader.TryGetInt64() |> fst then box (reader.GetInt64())
            else box (reader.GetDouble())
        | JsonTokenType.String -> box (reader.GetString())
        | _ ->
          raise <| JsonException()

    override _.Write(writer, value, options) =
        JsonSerializer.Serialize(writer, value, value.GetType(), options)

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
  jsOptions.Converters.Add(new ObjectAsConcreteTypeConverter())
  jsOptions.PropertyNamingPolicy <- JsonNamingPolicy.CamelCase
  let formatter = new SystemTextJsonFormatter()
  formatter.JsonSerializerOptions <- jsOptions

  formatter
