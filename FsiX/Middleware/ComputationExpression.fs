module FsiX.Middleware.ComputationExpression

open System
open Fantomas.Core.SyntaxOak
open Fantomas.Core

open FSharpPlus
open FsiX
open FsiX.Utils

let parse code =
    async {
        try
            let! res = CodeFormatter.ParseOakAsync(false, code)

            return
                Some
                    {| tree = res |> Array.head |> fst
                       hasTupleHack = false |}
        with _ ->
            try
                let! res =
                    String.trim [ ' ' ] code
                    |> fun code -> code + Environment.NewLine + "()"
                    |> fun code -> CodeFormatter.ParseOakAsync(false, code)

                return
                    Some
                        {| tree = res |> Array.head |> fst
                           hasTupleHack = true |}
            with _ ->
                return None
    }

let isCompExpr (parsed: Oak) =
    let parsed = parsed.ModulesOrNamespaces.Head |> _.Declarations

    let rec isExprComp =
        function
        | Expr.CompExprBody _ -> true
        | Expr.IfThenElse e -> isExprComp e.ElseExpr || isExprComp e.ThenExpr
        | Expr.IfThen e -> isExprComp e.ThenExpr
        | Expr.IfThenElif e -> e.Branches |> List.exists (fun e -> isExprComp e.ThenExpr)
        | Expr.For e -> isExprComp e.DoBody
        | Expr.ForEach e -> isExprComp e.BodyExpr
        | Expr.While e -> isExprComp e.DoExpr
        | Expr.Match e -> e.Clauses |> List.exists (fun e -> isExprComp e.BodyExpr)
        | Expr.MatchLambda e -> e.Clauses |> List.exists (fun e -> isExprComp e.BodyExpr)
        | _ -> false

    parsed
    |> List.exists (function
        | ModuleDecl.DeclExpr expr -> isExprComp expr
        | _ -> false)

let rewriteParsedExpr (parsed: Oak) =
    let wrapInCompExpr range (expr: Expr) =
        let mkText text = SingleTextNode(text, range)

        let runSyncUnit =
            LinkSingleAppUnit(
                Expr.OptVar
                <| ExprOptVarNode(false, IdentListNode([ IdentifierOrDot.Ident(mkText "Run") ], range), range),
                UnitNode(mkText "(", mkText ")", range),
                range
            )

        let mkParen expr =
            Expr.Paren(ExprParenNode(mkText "(", expr, mkText ")", range))

        ExprChain(
            [ ChainLink.Expr(mkParen expr)
              ChainLink.Dot(mkText ".")
              ChainLink.AppUnit runSyncUnit ],
            range
        )
        |> Expr.Chain

    let rec mapExpr =
        function
        | Expr.CompExprBody e ->
            Expr.CompExprBody
            <| ExprCompExprBodyNode(e.Statements |> List.map mapStatement, e.Range)
        | Expr.IfThenElse e ->
            Expr.IfThenElse
            <| ExprIfThenElseNode(e.If, e.IfExpr, e.Then, mapExpr e.ThenExpr, e.Else, mapExpr e.ElseExpr, e.Range)
        | Expr.IfThen e -> Expr.IfThen <| mapIfThen e
        | Expr.IfThenElif e ->
            Expr.IfThenElif
            <| ExprIfThenElifNode(e.Branches |> List.map mapIfThen, e.Else, e.Range)
        //| Expr.For e -> Expr.For <| ExprForNode(e.For, e.Range)
        | Expr.ForEach e ->
            Expr.ForEach
            <| ExprForEachNode(e.For, e.Pattern, e.EnumExpr, e.IsArrow, mapExpr e.BodyExpr, e.Range)
        | Expr.While e -> Expr.While <| ExprWhileNode(e.While, e.WhileExpr, mapExpr e.DoExpr, e.Range)
        | Expr.Match e ->
            Expr.Match
            <| ExprMatchNode(e.Match, e.MatchExpr, e.With, mapClauses e.Clauses, e.Range)
        | Expr.MatchLambda e ->
            Expr.MatchLambda
            <| ExprMatchLambdaNode(e.Function, mapClauses e.Clauses, e.Range)
        | x -> x

    and mapIfThen (e: ExprIfThenNode) =
        ExprIfThenNode(e.If, e.IfExpr, e.Then, mapExpr e.ThenExpr, e.Range)

    and mapClauses =
        List.map
        <| fun (x: MatchClauseNode) ->
            MatchClauseNode(x.Bar, x.Pattern, x.WhenExpr, x.Arrow, mapExpr x.BodyExpr, x.Range)

    and mapStatement =
        function
        | ComputationExpressionStatement.LetOrUseBangStatement e when e.LeadingKeyword.Text = "let!" ->
            ComputationExpressionStatement.LetOrUseBangStatement(
                ExprLetOrUseBangNode(
                    SingleTextNode("let", e.LeadingKeyword.Range),
                    e.Pattern,
                    e.Equals,
                    wrapInCompExpr e.Range e.Expression,
                    e.Range
                )
            )
        | ComputationExpressionStatement.LetOrUseBangStatement e when e.LeadingKeyword.Text = "use!" ->
            ComputationExpressionStatement.LetOrUseBangStatement(
                ExprLetOrUseBangNode(
                    SingleTextNode("use", e.LeadingKeyword.Range),
                    e.Pattern,
                    e.Equals,
                    wrapInCompExpr e.Range e.Expression,
                    e.Range
                )
            )
        | ComputationExpressionStatement.AndBangStatement e ->
            ComputationExpressionStatement.LetOrUseBangStatement(
                ExprLetOrUseBangNode(
                    SingleTextNode("let", e.LeadingKeyword.Range),
                    e.Pattern,
                    e.Equals,
                    wrapInCompExpr e.Range e.Expression,
                    e.Range
                )
            )
        | ComputationExpressionStatement.OtherStatement(Expr.Single e) ->
            let mkSingle keyword expr =
                let leading = SingleTextNode(keyword, e.Range)
                Expr.Single(ExprSingleNode(leading, e.AddSpace, e.SupportsStroustrup, expr, e.Range))

            let newStatement =
                match e.Leading.Text with
                | "do!" -> mkSingle "do" (wrapInCompExpr e.Range e.Expr)
                | "match!" -> mkSingle "match" (wrapInCompExpr e.Range e.Expr)
                | "return!" -> wrapInCompExpr e.Range e.Expr
                | "return" -> e.Expr
                | _ -> Expr.Single e

            ComputationExpressionStatement.OtherStatement newStatement
        | ComputationExpressionStatement.OtherStatement e -> mapExpr e |> ComputationExpressionStatement.OtherStatement

    let mapDecl =
        function
        | ModuleDecl.DeclExpr e -> ModuleDecl.DeclExpr(mapExpr e)
        | x -> x

    let namespaces =
        parsed.ModulesOrNamespaces
        |> List.map (fun x -> ModuleOrNamespaceNode(x.Header, List.map mapDecl x.Declarations, x.Range))

    Oak(parsed.ParsedHashDirectives, namespaces, parsed.Range)

open FsiX.AppState

let rewriteCompExpr (logger: ILogger) code =
    async {
        let! parsed = parse code

        match parsed with
        | None -> return code
        | Some parsed ->
            if not <| isCompExpr parsed.tree then
                return code
            else
                let rewrittenAst = rewriteParsedExpr parsed.tree
                let! code = CodeFormatter.FormatOakAsync rewrittenAst |>> String.trimEnd (" " + Environment.NewLine)

                let logCode code = 
                  logger.LogDebug $"Rewriting user computation expression input to: \n {code}"

                if parsed.hasTupleHack then
                    let code = code.Substring(0, code.Length - 2)
                    logCode code
                    return code
                else
                    logCode code
                    return code
    }
let compExprMiddleware next (request, st: AppState) =
  let compExprFlagEnabled =
    match st.Session.TryFindBoundValue "_fsiXCompExpr" with
    | Some fsiBoundValue when fsiBoundValue.Value.ReflectionValue = true -> true
    | _ -> false
  let shouldRunCompExpr (m: Map<string, obj>) =
    match compExprFlagEnabled, Map.tryFind "simplifyCompExpression" m with
    | _, Some v when v = true -> true
    | true, None -> true
    | _ -> false
  match request with
  | {Args = m} when shouldRunCompExpr m ->
      let rewritten = rewriteCompExpr st.Logger request.Code |> Async.RunSynchronously
      next ({request with Code = rewritten}, st)
  | _ -> next (request, st)
