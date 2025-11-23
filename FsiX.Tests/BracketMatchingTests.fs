module FsiX.Tests.BracketMatchingTests

open Expecto
open FSharp.Compiler.Text
open FSharp.Compiler.Tokenization

open FsiX.Features.SyntaxHighlighting

#nowarn "0057"

let tokenize s =
  let acc = ResizeArray()
  FSharpLexer.Tokenize(SourceText.ofString s, acc.Add)
  List.ofSeq acc

let dummyLineLengths = Seq.initInfinite (fun _ -> 0)

[<Tests>]
let tests =
  testList "bracket matching tests"
    [ testCase "test match"
      <| fun _ -> (tokenize "[]" |> matchBrackets dummyLineLengths |> Expect.isNonEmpty) "[]"
      testCase "test mismatch"
      <| fun _ ->
        (tokenize "(]" |> matchBrackets dummyLineLengths |> Expect.isEmpty) "(]"
        (tokenize ")" |> matchBrackets dummyLineLengths |> Expect.isEmpty) ")"
      testCase "test ends with non-bracket"
      <| fun _ -> (tokenize "(foo" |> matchBrackets dummyLineLengths |> Expect.isEmpty) "(foo"]
