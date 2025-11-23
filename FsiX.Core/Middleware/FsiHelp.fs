// Rewritten from Compiler/Interactive/fsihelp.fs, with added support
// for types, overloads resolution and BCL members
module internal FsiX.Middleware.FsiHelp

open System
open System.IO
open System.Xml
open System.Reflection
open System.Collections.Generic
open System.Collections.Concurrent
open System.Text.RegularExpressions

open FSharp.Reflection
open FSharp.Quotations.Patterns
open FSharp.Compiler.Interactive.FsiHelp.Parser

let private cleanupXmlContent (s: string) = s.Replace("\n ", "\n").Trim()

let private trimDotNet (s: string) =
    let s = if s.Length > 2 && s[1] = ':' then s.Substring(2) else s
    let idx = s.IndexOf('`')
    let s = if idx > 0 then s.Substring(0, idx) else s
    s

let private xmlDocCache = ConcurrentDictionary<string, Lazy<XmlDocument>>()

let private tryGetXmlDocument xmlPath =
    let valueFactory xmlPath =
        lazy
            let rawXml = File.ReadAllText(xmlPath)
            let xmlDocument = XmlDocument()
            xmlDocument.LoadXml(rawXml)
            xmlDocument

    try
        Some(xmlDocCache.GetOrAdd(xmlPath, valueFactory).Value)
    with _ ->
        None

let private getTexts (node: XmlNode) =
    seq {
        for child in node.ChildNodes do
            match child.Name with
            | "#text" ->
                yield child.Value
            | "c" ->
                yield child.InnerText
            | "see" ->
                let cref = child.Attributes.GetNamedItem("cref")

                if not (isNull cref) then
                    yield cref.Value |> trimDotNet
    }
    |> String.concat ""

let private tryXmlNodeToHelp assembly fullName  (node: XmlNode) =
    let summary =
        node.SelectSingleNode("summary")
        |> Option.ofObj
        |> Option.map getTexts
        |> Option.map cleanupXmlContent

    let remarks =
        node.SelectSingleNode("remarks")
        |> Option.ofObj
        |> Option.map getTexts
        |> Option.map cleanupXmlContent

    let parameters =
        node.SelectNodes("param")
        |> Seq.cast<XmlNode>
        |> Seq.map (fun n -> n.Attributes.GetNamedItem("name").Value.Trim(), n.InnerText.Trim())
        |> List.ofSeq

    let returns =
        node.SelectSingleNode("returns")
        |> Option.ofObj
        |> Option.map (getTexts >> _.Trim())

    let exceptions =
        node.SelectNodes("exception")
        |> Seq.cast<XmlNode>
        |> Seq.map (fun n ->
            let exType = n.Attributes.GetNamedItem("cref").Value
            let idx = exType.IndexOf(':')
            let exType = if idx >= 0 then exType.Substring(idx + 1) else exType
            exType.Trim(), n.InnerText.Trim())
        |> List.ofSeq

    let examples =
        node.SelectNodes("example")
        |> Seq.cast<XmlNode>
        |> Seq.map (fun n ->
            let codeNode = n.SelectSingleNode("code")

            let code =
                if isNull codeNode then
                    ""
                else
                    n.RemoveChild(codeNode) |> ignore
                    cleanupXmlContent codeNode.InnerText

            code, cleanupXmlContent n.InnerText)
        |> List.ofSeq

    summary
    |> Option.map (fun s ->
        {
            Summary = s
            Remarks = remarks
            Parameters = parameters
            Returns = returns
            Exceptions = exceptions
            Examples = examples
            FullName = fullName
            Assembly = assembly
        })

let private refPath =
    let rec times f n x = if n > 0 then times f (n - 1) (f x) else x
    let dotnetPath = typeof<obj>.Assembly.Location |> times Path.GetDirectoryName 4
    let frameworkVersion =
        Regex.Match(string Environment.Version, @"(\d+\.\d+).+").Groups[1] |> sprintf "net%O"
    Path.Combine(dotnetPath, "packs", "Microsoft.NETCore.App.Ref", string Environment.Version, "ref", frameworkVersion)

let private getXmlPath dllLocation =
    let xmlPath = Path.ChangeExtension(dllLocation, ".xml")
    
    let xmlPath =
        if File.Exists(xmlPath) |> not then
            Path.Combine(refPath, Path.GetFileName(xmlPath))
        else xmlPath
    
    let xmlPath =
        if File.Exists(xmlPath) |> not && Path.GetFileName(xmlPath) = "System.Private.CoreLib.xml" then
            Path.Combine(Path.GetDirectoryName(xmlPath), "System.Runtime.xml")
        else xmlPath
    
    xmlPath


let rec private formatGenericType (genericParams: IReadOnlyDictionary<Type, string>) (ty: Type) =
    let stripGenericAnnotation s =
        Regex.Match(s, @"(.+?)`\d+").Groups[1] |> string

    if ty.IsGenericParameter then genericParams[ty]
    else if not ty.IsGenericType then ty.FullName
    else
        ty.GetGenericArguments()
        |> Seq.map (formatGenericType genericParams)
        |> String.concat ","
        |> sprintf "%s.%s{%s}" ty.Namespace (stripGenericAnnotation ty.Name)

let private getTypeName (ty: Type) =
    if ty.IsGenericType then ty.GetGenericTypeDefinition() else ty
    |> _.FullName.Replace("+", ".")

let private getMemberXmlDocIdPrefix (mem: MemberInfo) =
    match mem.MemberType with
    | MemberTypes.Event -> "E:"
    | MemberTypes.Field -> "F:"
    | MemberTypes.Property -> "P:"
    | MemberTypes.Method
    | MemberTypes.Constructor -> "M:"
    | _ -> ""

let private getMemberXmlDocId (mem: MemberInfo) =
    let prefix = getMemberXmlDocIdPrefix mem

    let body =
        match mem.MemberType with
        | MemberTypes.Field
        | MemberTypes.Event -> $"{getTypeName mem.DeclaringType}.{mem.Name}"
        | MemberTypes.Constructor
        | MemberTypes.Method
        | MemberTypes.Property ->
            let parameters, genericParams =
                match mem with
                | :? MethodInfo as m ->
                    let m = if m.IsGenericMethod then m.GetGenericMethodDefinition() else m
                    m.GetParameters(), m.GetGenericArguments()
                | :? ConstructorInfo as m -> m.GetParameters(), [||]
                | :? PropertyInfo as p -> p.GetIndexParameters(), [||]
                | _ -> [||], [||]

            let memberName =
                if genericParams.Length = 0 then mem.Name
                else $"{mem.Name}``{genericParams.Length}"
                |> _.Replace(".", "#")
                |> sprintf "%s.%s" (getTypeName mem.DeclaringType)

            if parameters.Length = 0 then memberName
            else
                let genericParams =
                    genericParams
                    |> Seq.mapi (fun i ty -> ty, $"``{i}")
                    |> Seq.append (
                        mem.DeclaringType.GetGenericArguments() |> Seq.mapi (fun i ty -> ty, $"`{i}")
                    )
                    |> readOnlyDict

                parameters
                |> Seq.map (_.ParameterType >> formatGenericType genericParams)
                |> String.concat ","
                |> sprintf "%s(%s)" memberName
        | _ -> ""

    prefix + body

let private getFuzzyMemberXmlDocId (mem: MemberInfo) =
    let prefix = getMemberXmlDocIdPrefix mem
    let qual = getTypeName mem.DeclaringType
    let name = mem.Name.Replace('.', '#')

    $"{prefix}{qual}.{name}"

type HelpEntry =
    | Member of MemberInfo
    | Type of Type
    | UnionCase of UnionCaseInfo

let private getDeclaringType = function
    | Member mem -> mem.DeclaringType
    | Type ty -> ty
    | UnionCase case -> case.DeclaringType

let private getXmlDocId = function
    | Member mem -> getMemberXmlDocId mem
    | Type ty -> $"T:{getTypeName ty}"
    | UnionCase case -> $"T:{getTypeName case.DeclaringType}.{case.Name}"

let private getSourceName = function
    | Member(:? MethodInfo as m) ->
        try
            let attr = m.GetCustomAttribute<CompilationSourceNameAttribute>()
            attr.SourceName
        with _ -> m.Name
    | Member mem -> mem.Name
    | Type ty -> ty.Name
    | UnionCase case -> case.Name

let tryMkHelp entry =
    let xmlDocId = getXmlDocId entry
    let declaringType = getDeclaringType entry
    let sourceName = getSourceName entry

    declaringType.Assembly.Location
    |> getXmlPath
    |> tryGetXmlDocument
    |> Option.map (fun node ->
        node.SelectSingleNode($"/doc/members/member[@name=\"{xmlDocId}\"]")
        |> Option.ofObj
        |> Option.orElseWith (fun () ->
            match entry with
            | Member mem ->
                getFuzzyMemberXmlDocId mem
                |> sprintf "/doc/members/member[contains(@name, \"%s\")]"
                |> node.SelectSingleNode
                |> Option.ofObj
            | _ -> None))
    |> Option.flatten
    |> Option.bind (fun node ->
        let fullName = $"{getTypeName declaringType}.{sourceName}"
        tryXmlNodeToHelp declaringType.Assembly.Location fullName node)

let rec private exprNames expr =
    match expr with
    | Call(None, methodInfo, _exprList) -> Some(Member methodInfo)
    | Lambda(_param, body) -> exprNames body
    | Let(_, _, body) -> exprNames body
    | Value(_o, t) -> Some(Type t)
    | DefaultValue t -> Some(Type t)
    | PropertyGet(_o, info, _) -> Some(Member info)
    | NewUnionCase(info, _exprList) -> Some(UnionCase info)
    | NewObject(ctorInfo, _e) -> Some(Member ctorInfo)
    | NewArray(t, _exprs) -> Some(Type t)
    | NewTuple _ -> Some(Type typedefof<_ * _>)
    | NewStructTuple _ -> Some(Type typedefof<struct(_ * _)>)
    | _ -> None

let tryGetHelp (expr: Quotations.Expr) = expr |> exprNames |> Option.bind tryMkHelp
