module FsiX.Middleware.Directives

#nowarn "57"

open System.IO
open FSharpPlus
open Fantomas.Core
open Fantomas.FCS.Syntax
open FsiX.AppState
open FsiX.Utils
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
  open System.Reflection
  open System.Text.RegularExpressions

  [<AutoOpen>]
  module private Reflection =
    let fcs = sprintf "FSharp.Compiler.Interactive.FsiHelp+%s, FSharp.Compiler.Service"
    let staticNonPublic = BindingFlags.Static ||| BindingFlags.NonPublic

    let parserModule = Type.GetType(fcs "Parser")
    let operatorModule = Type.GetType("Microsoft.FSharp.Core.Operators, FSharp.Core")

    let tryMkHelpMethod = parserModule.GetMethod("tryMkHelp", staticNonPublic)
    let tryGetXmlDocumentMethod = parserModule.GetMethod("tryGetXmlDocument", staticNonPublic)
    let raiseMethod = operatorModule.GetMethod("Raise")

  open Microsoft.FSharp.Quotations.Patterns
  // Adapted from Compiler/Interactive/fsihelp.fs
  module Expr =
    let tryGetSourceName (methodInfo: MethodInfo) =
        try
            let attr = methodInfo.GetCustomAttribute<CompilationSourceNameAttribute>()
            Some attr.SourceName
        with _ ->
            None

    let refPath =
      let rec times f n x = if n > 0 then times f (n - 1) (f x) else x
      let dotnetPath = typeof<obj>.Assembly.Location |> times Path.GetDirectoryName 4
      let frameworkVersion =
        Regex.Match(string Environment.Version, @"(\d+\.\d+).+").Groups[1] |> sprintf "net%O"
      Path.Combine(dotnetPath, "packs", "Microsoft.NETCore.App.Ref", string Environment.Version, "ref", frameworkVersion)

    let getInfos (declaringType: Type) (sourceName: string option) (implName: string) =
        let xmlPath = Path.ChangeExtension(declaringType.Assembly.Location, ".xml")
        let xmlPath =
          if File.Exists(xmlPath) |> not then
            Path.Combine(refPath, Path.GetFileName(xmlPath))
          else xmlPath
        let xmlPath =
          if File.Exists(xmlPath) |> not && Path.GetFileName(xmlPath) = "System.Private.CoreLib.xml" then
            Path.Combine(Path.GetDirectoryName(xmlPath), "System.Runtime.xml")
          else xmlPath
        let xmlDoc = tryGetXmlDocumentMethod.Invoke(null, [|xmlPath|]) |> unbox<Xml.XmlDocument option>
        let assembly = Path.GetFileName(declaringType.Assembly.Location)

        // for FullName cases like Microsoft.FSharp.Core.FSharpOption`1[System.Object]
        let fullName =
            let idx = declaringType.FullName.IndexOf('[')

            if idx >= 0 then
                declaringType.FullName.Substring(0, idx)
            else
                declaringType.FullName

        let fullName = fullName.Replace('+', '.') // for FullName cases like Microsoft.FSharp.Collections.ArrayModule+Parallel

        (xmlDoc, assembly, fullName, implName, sourceName |> Option.defaultValue implName)

    let rec exprNames expr =
        match expr with
        | Call(exprOpt, methodInfo, _exprList) ->
            match exprOpt with
            | Some _ -> None
            | None ->
                let sourceName = tryGetSourceName methodInfo
                getInfos methodInfo.DeclaringType sourceName methodInfo.Name |> Some
        | Lambda(_param, body) -> exprNames body
        | Let(_, _, body) -> exprNames body
        | Value(_o, t) -> getInfos t (Some t.Name) t.Name |> Some
        | DefaultValue t -> getInfos t (Some t.Name) t.Name |> Some
        | PropertyGet(_o, info, _) -> getInfos info.DeclaringType (Some info.Name) info.Name |> Some
        | NewUnionCase(info, _exprList) -> getInfos info.DeclaringType (Some info.Name) info.Name |> Some
        | NewObject(ctorInfo, _e) -> getInfos ctorInfo.DeclaringType (Some ctorInfo.Name) ctorInfo.Name |> Some
        | NewArray(t, _exprs) -> getInfos t (Some t.Name) t.Name |> Some
        | NewTuple _ ->
            let ty = typeof<_ * _>
            getInfos ty (Some ty.Name) ty.Name |> Some
        | NewStructTuple _ ->
            let ty = typeof<struct (_ * _)>
            getInfos ty (Some ty.Name) ty.Name |> Some
        | _ -> None

  open FSharp.Compiler.Interactive

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
        match Expr.exprNames e with
        | Some(xmlDocument, assembly, modName, implName, sourceName) ->
          tryMkHelpMethod.Invoke(null, [| xmlDocument; assembly; modName; implName; sourceName |])
          |> unbox<FsiHelp.Parser.Help voption>
          |> ValueOption.map _.ToDisplayString()
          |> ValueOption.defaultValue failMsg
        | _ -> failMsg
      | _ -> failMsg
      |> st.OutStream.WriteLine
      next ({request with Code = ""}, st)
    | None -> next (request, st)

  let htypeDirectiveMiddleware next (request, st) =
    match parseHtypeDirective request.Code with
    | Some code ->
      match st.Session.EvalExpressionDirectly($"typeof<{code}>") with
      | Some(:? Type as t) ->
        let xmlDocument, assembly, modName, _, sourceName = Expr.getInfos t None t.Name
        // Assume it's scoped in a namespace
        let m = Regex.Match(modName, @"^(.+?)\.([^\.]+)$")
        if m.Success then
          let modName = string m.Groups[1]
          let implName = string m.Groups[2]
          tryMkHelpMethod.Invoke(null, [| xmlDocument; assembly; modName; implName; sourceName |])
          |> unbox<FsiHelp.Parser.Help voption>
          |> ValueOption.map _.ToDisplayString()
          |> ValueOption.defaultValue failMsg
        else failMsg
      | _ -> failMsg
      |> st.OutStream.WriteLine
      next ({request with Code = ""}, st)
    | None -> next (request, st)

let viBindMiddleware next (request, st) = 
  let trimmed = request.Code.TrimStart()
  if trimmed.StartsWith(':') then next ({request with Code = "#" + trimmed[1..]}, st)
  else next (request, st)
