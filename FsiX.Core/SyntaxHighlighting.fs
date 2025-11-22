module FsiX.Features.SyntaxHighlighting

open System

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
        FormatSpan(rangeToSpan lineLengths token.Range, ConsoleFormat(Nullable color))
        |> Some
    if token.IsIdentifier then mkSpan AnsiColor.BrightBlue
    else if token.IsControlKeyword then mkSpan AnsiColor.BrightRed
    else if token.IsKeyword && token.Kind <> FSharpTokenKind.OffsideEnd then mkSpan AnsiColor.Blue
    else if token.IsNumericLiteral then mkSpan AnsiColor.BrightGreen
    else if token.IsStringLiteral then mkSpan AnsiColor.Red
    else None

let private leftBracketsToRight, private rightBracketsToLeft =
    let pairs =
        [ FSharpTokenKind.LeftBrace, FSharpTokenKind.RightBrace
          FSharpTokenKind.LeftBraceBar, FSharpTokenKind.BarRightBrace
          FSharpTokenKind.LeftBracket, FSharpTokenKind.RightBracket
          FSharpTokenKind.LeftBracketBar, FSharpTokenKind.BarRightBracket
          FSharpTokenKind.LeftParenthesis, FSharpTokenKind.RightParenthesis
          FSharpTokenKind.LeftBracketLess, FSharpTokenKind.GreaterRightBracket
          FSharpTokenKind.LeftQuote, FSharpTokenKind.RightQuote ]
    dict pairs, pairs |> Seq.map (fun (x, y) -> y, x) |> dict

let matchBrackets lineLengths tokens =
    let rec loop (stack: FSharpToken list) (tokens: FSharpToken list) =
        let error range rest =
            seq {
                FormatSpan(rangeToSpan lineLengths range, ConsoleFormat(Nullable AnsiColor.Red))
                yield! loop stack rest
            }
        match tokens with
        | [last] ->
            match rightBracketsToLeft.TryGetValue(last.Kind) with
            | true, expected ->
                match stack with
                | hd :: _ when hd.Kind = expected ->
                    [hd; last]
                    |> Seq.map (fun token ->
                        FormatSpan(
                            rangeToSpan lineLengths token.Range,
                            ConsoleFormat(Nullable AnsiColor.BrightMagenta
                        )))
                | _ -> error last.Range []
            | false, _ -> Seq.empty
        | token :: restTokens ->
            if leftBracketsToRight.ContainsKey(token.Kind) then
                loop (token :: stack) restTokens
            else
                match rightBracketsToLeft.TryGetValue(token.Kind) with
                | true, expected ->
                    match stack with
                    | [] -> error token.Range restTokens
                    | hd :: _ when hd.Kind <> expected -> error token.Range restTokens
                    | _ :: tl -> loop tl restTokens
                | false, _ -> loop stack restTokens
        | [] -> Seq.empty

    loop [] tokens

let getFormatSpans (text: string) =
    // FSharp.Compiler.WarnScopes.parseDirective crushes when the input quoted string is incomplete
    let trimmed = text.TrimStart()
    if trimmed.StartsWith("#nowarn") || trimmed.StartsWith("#warnon") then FSharpPlus.IReadOnlyCollection.empty
    else
    let lineLengths = text.Split('\n') |> Seq.map (String.length >> (+) 1) |> Seq.toArray
    let tokens = ResizeArray()
    let noLexFilter = FSharpLexerFlags.Default &&& ~~~FSharpLexerFlags.UseLexFilter
    FSharpLexer.Tokenize(SourceText.ofString text, tokens.Add, flags = noLexFilter)
    seq {
        yield! tokens |> Seq.choose (toFormatSpan lineLengths)
        yield! matchBrackets lineLengths (List.ofSeq tokens)
    }
    |> FSharpPlus.IReadOnlyCollection.ofSeq
