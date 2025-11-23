// HACK
// Access F# compiler's internal APIs via reflection
// Dangerous as compiler's internal implementation may change

[<AutoOpen>]
module FsiX.Middleware.FsiEvaluationSession

open System
open System.Reflection

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Interactive.Shell

[<AutoOpen>]
module private Reflection =
    let fcs = sprintf "FSharp.Compiler.Interactive.Shell+%s, FSharp.Compiler.Service"
    let instanceNonPublic = BindingFlags.Instance ||| BindingFlags.NonPublic

    let fsiInteractionProcessorType = Type.GetType(fcs "FsiInteractionProcessor")
    let fsiDynamicCompilerType = Type.GetType(fcs "FsiDynamicCompiler")
    let fsiInteractionStepStatusType = Type.GetType(fcs "FsiInteractionStepStatus")
    let fsiDynamicCompilerStateType = Type.GetType(fcs "FsiDynamicCompilerState")
    let resultTupleType = typedefof<_ * _>.MakeGenericType(fsiDynamicCompilerStateType, fsiInteractionStepStatusType)
    let completedType = Type.GetType(fcs "FsiInteractionStepStatus+Completed")

    let diagnosticsLoggerField =
        typeof<FsiEvaluationSession>.GetField("diagnosticsLogger", instanceNonPublic)
    let fsiInteractionProcessorField =
        typeof<FsiEvaluationSession>.GetField("fsiInteractionProcessor", instanceNonPublic)
    let currStateField =
        fsiInteractionProcessorType.GetField("currState", instanceNonPublic)
    let fsiDynamicCompilerField =
        fsiInteractionProcessorType.GetField("fsiDynamicCompiler", instanceNonPublic)

    let evalParsedExpressionMethod =
        fsiDynamicCompilerType.GetMethod("EvalParsedExpression", instanceNonPublic)
    let item2Getter = resultTupleType.GetMethod("get_Item2")
    let itemGetter = completedType.GetMethod("get_Item", instanceNonPublic)

let private parseExpr (checker: FSharpChecker) code fileName =
    async {
        let source = SourceText.ofString code
        let! projectOptions, _ = checker.GetProjectOptionsFromScript(fileName, source)
        let parsingOptions, _ = checker.GetParsingOptionsFromProjectOptions(projectOptions)
        let! results = checker.ParseFile(fileName, source, parsingOptions)
        return
            match results.ParseHadErrors, results.ParseTree with
            | false, ParsedInput.ImplFile(
                ParsedImplFileInput(
                    contents = [SynModuleOrNamespace(decls = [SynModuleDecl.Expr(expr = e)])]
                )) -> Some e
            | _ -> None
    }
    |> Async.RunSynchronously

// Evaluate the given code directly, without logging or storing in it
type FsiEvaluationSession with
    member this.EvalExpressionDirectly(code) =
        this.EvalExpressionDirectly(code, "input.fsx")

    member this.EvalExpressionDirectly(code, scriptFileName) =
        let fsiInteractionProcessor = fsiInteractionProcessorField.GetValue(this)
        let fsiDynamicCompiler = fsiDynamicCompilerField.GetValue(fsiInteractionProcessor)
        let istate = currStateField.GetValue(fsiInteractionProcessor)
        let diagnosticsLogger = diagnosticsLoggerField.GetValue(this)
        let ctok = null

        parseExpr this.InteractiveChecker code scriptFileName
        |> Option.bind (fun expr ->
            evalParsedExpressionMethod.Invoke(
                fsiDynamicCompiler,
                [| ctok
                   diagnosticsLogger
                   istate
                   expr
                   true |]
            )
            |> fun results -> item2Getter.Invoke(results, [||])
            |> fun status ->
                if status.GetType() = completedType then
                    itemGetter.Invoke(status, [||])
                    |> unbox<FsiValue option>
                    |> Option.map _.ReflectionValue
                else None
        )
