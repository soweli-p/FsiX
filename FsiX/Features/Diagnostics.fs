module FsiX.Features.Diagnostics

open FSharp.Compiler.EditorServices
open FSharp.Compiler.Interactive
open FSharp.Compiler.Text
open FSharp.Compiler.Diagnostics


type Range = {
  StartLine: int
  StartColumn: int
  EndLine: int
  EndColumn: int
}
type DiagnosticSeverity = Error | Hidden | Info | Warning
type Diagnostic = {
  Message: string
  Subcategory: string
  Range: Range
  Severity: DiagnosticSeverity } with
  static member mkDiagnostic (fsDiagnostic: FSharpDiagnostic) =
    let mapSeverity = function
      | FSharpDiagnosticSeverity.Error -> Error
      | FSharpDiagnosticSeverity.Hidden -> Hidden
      | FSharpDiagnosticSeverity.Info -> Info
      | FSharpDiagnosticSeverity.Warning -> Warning
    let range = fsDiagnostic.Range
    { Message = fsDiagnostic.Message
      Severity = mapSeverity fsDiagnostic.Severity;
      Subcategory = fsDiagnostic.Subcategory
      Range = { StartLine = range.StartLine
                StartColumn = range.StartColumn
                EndLine = range.EndLine
                EndColumn = range.EndColumn } }

let getDiagnostics (fsiSession: Shell.FsiEvaluationSession) text =
    let parse, typed, glob = fsiSession.ParseAndCheckInteraction text
    parse.Diagnostics
    |> Seq.append typed.Diagnostics
    |> Seq.append glob.Diagnostics
    |> Seq.map Diagnostic.mkDiagnostic
    |> Seq.distinct
    |> Seq.toArray
