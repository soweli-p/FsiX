module FsiX.Middleware.Directives

#nowarn "57"

open System.IO
open Fantomas.Core
open Fantomas.FCS.Syntax
open FsiX.AppState
open FsiX.Parsers

module OpenDirective =
  let openedFileKey = "openedFiles"
  let openDirectiveMetadata = "openDirectiveMetadata"

  type OpenedFiles = string Set

  let openDirectiveMiddleware next (request, st) =
    let moduleNameOfPath (path: string) =
      path
      |> System.IO.Path.GetFileNameWithoutExtension
      |> function
        | "" -> ""
        | s -> $"{System.Char.ToUpper s[0]}{s[1..]}"
    let openDirectiveLines fileToOpen =
      let fileToOpen = Path.GetFullPath fileToOpen
      let file = File.ReadAllText fileToOpen

      let [|res, _|] = CodeFormatter.ParseAsync(false, file) |> Async.RunSynchronously

      let (ParsedInput.ImplFile(ParsedImplFileInput(contents = contents))) = res

      let [ SynModuleOrNamespace(decls = codeLines; longId = _) ] = contents

      let runOpen (l: LongIdent) =
          let path =
              l |>  Seq.map _.idText |> Seq.toList |> String.concat "."
          $"open {path}"

      let chooseFn = function
        | SynModuleDecl.Open(target = target) ->
          match target with
          | SynOpenDeclTarget.ModuleOrNamespace(longId = l) -> Some <| runOpen l.LongIdent
          | SynOpenDeclTarget.Type(typeName = t) -> None //todo
        | _ -> None
      $"open {moduleNameOfPath fileToOpen}" :: List.choose chooseFn codeLines
    let hasOpenedFile fileName = 
      match st.Custom.TryFind openedFileKey with
      | None -> false
      | Some openedFileSet ->
          openedFileSet :?> OpenedFiles |> Set.contains fileName
    let addOpenedFile st fileName =
      let changeFn: obj option -> obj option = function
        | None -> Set.ofList [fileName] :> obj |> Some
        | Some set -> set :?> OpenedFiles |> Set.add fileName :> obj |> Some
      {st with Custom = Map.change openedFileKey changeFn st.Custom}
    let addMetadata response codeLines =
      {response with Metadata = response.Metadata.Add(openDirectiveMetadata, String.concat "\n" codeLines)}

    match request with
    | {Code = code; Args = args; } when args.ContainsKey "fileName" ->
      let fileName = args["fileName"]
      if hasOpenedFile fileName then
        next (request, st)
      else 
      let lines = openDirectiveLines (args["fileName"])
      let code = lines @ [code] |> String.concat "\n"
      let response, st = next ({request with Code = code}, st)
      addMetadata response lines, addOpenedFile st fileName
    | {Code = code} ->
      match parseOpenDirective code with
      | Some paths ->
        paths
        |> Seq.distinct
        |> Seq.collect (fun path ->
          st.Session.EvalScriptNonThrowing path |> ignore
          openDirectiveLines path)
        |> fun lines ->
          let code = String.concat "\n" lines
          printfn "%s" code
          let response, st = next ({request with Code = code}, st)
          addMetadata response lines, paths |> Seq.fold addOpenedFile st 
      | _ -> next (request, st)
  
module HelpDirective =
  open System
  open Microsoft.FSharp.Quotations.Patterns

  let operatorModule = Type.GetType("Microsoft.FSharp.Core.Operators, FSharp.Core")
  let raiseMethod = operatorModule.GetMethod("Raise")

  let failMsg = "unable to get documentation"
  
  let helpDirectiveMiddleware next (request, st) =
    match parseHelpDirective request.Code with
    | Some code ->
      match st.Session.EvalExpressionDirectly($"<@@ {code} @@>") with
      // Evaluates to a Call(None, Raise, Value 1) when compilation fails
      | Some(:? FSharp.Quotations.Expr as Call(None, m, [Value(:? int as 1, _)]))
        when m.GetGenericMethodDefinition() = raiseMethod ->
        failMsg
      | Some(:? FSharp.Quotations.Expr as e) ->
        FsiHelp.tryGetHelp e
        |> Option.map _.ToDisplayString()
        |> Option.defaultValue failMsg
      | _ -> failMsg
      |> st.OutStream.WriteLine
      next ({request with Code = ""}, st)
    | None -> next (request, st)

  let htypeDirectiveMiddleware next (request, st) =
    match parseHtypeDirective request.Code with
    | Some code ->
      match st.Session.EvalExpressionDirectly($"typedefof<{code}>") with
      | Some(:? Type as t) ->
        FsiHelp.tryMkHelp (FsiHelp.Type t)
        |> Option.map _.ToDisplayString()
        |> Option.defaultValue failMsg
      | _ -> failMsg
      |> st.OutStream.WriteLine
      next ({request with Code = ""}, st)
    | None -> next (request, st)

let viBindMiddleware next (request, st) = 
  let trimmed = request.Code.TrimStart()
  if trimmed.StartsWith(':') then next ({request with Code = "#" + trimmed[1..]}, st)
  else next (request, st)
