module FsiX.SyntaxHighlighting

open System.Collections.Generic

open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization

open PrettyPrompt.Documents
open PrettyPrompt.Highlighting

#nowarn "0057"


type private FSharpToken with
    member this.IsControlKeyword =
        match this.Kind with
        | FSharpTokenKind.OffsideDo
        | FSharpTokenKind.OffsideElse
        | FSharpTokenKind.OffsideThen
        | FSharpTokenKind.Match
        | FSharpTokenKind.MatchBang
        | FSharpTokenKind.If
        | FSharpTokenKind.Then
        | FSharpTokenKind.Elif
        | FSharpTokenKind.Else
        | FSharpTokenKind.Assert
        | FSharpTokenKind.While
        | FSharpTokenKind.WhileBang
        | FSharpTokenKind.Do
        | FSharpTokenKind.DoBang
        | FSharpTokenKind.For
        | FSharpTokenKind.To
        | FSharpTokenKind.DownTo
        | FSharpTokenKind.Try
        | FSharpTokenKind.Yield
        | FSharpTokenKind.YieldBang -> true
        | _ -> false

let private posToIndex lineLengths (p: pos) = (lineLengths |> Seq.take (p.Line - 1) |> Seq.sum) +  p.Column

let private rangeToSpan lineLengths (r: range) =
    let start = posToIndex lineLengths r.Start
    TextSpan(start, posToIndex lineLengths r.End - start)

let private toFormatSpan lineLengths (token: FSharpToken) =
    let mkSpan (color: AnsiColor) =
        FormatSpan(rangeToSpan lineLengths token.Range, ConsoleFormat(System.Nullable color))
        |> Some
    if token.IsIdentifier then mkSpan AnsiColor.BrightBlue
    else if token.IsControlKeyword then mkSpan AnsiColor.BrightRed
    else if token.IsKeyword then mkSpan AnsiColor.Blue
    else if token.IsNumericLiteral then mkSpan AnsiColor.BrightGreen
    else if token.IsStringLiteral then mkSpan AnsiColor.Red
    else None

let getFormatSpans (text: string) =
    let lineLengths = text.Split('\n') |> Seq.map (String.length >> (+) 1) |> Seq.toArray
    let tokens = ResizeArray()
    FSharpLexer.Tokenize(
        SourceText.ofString text,
        (fun token -> toFormatSpan lineLengths token |> Option.iter tokens.Add))
    tokens :> IReadOnlyCollection<_>
