module internal FsiX.Parsers

open FParsec

let quotedStringOpen =
    let unescapedChar = noneOf "\\\""

    let escapedChar =
        [ "\\\"", '"'
          "\\\\", '\\'
          "\\/", '/'
          "\\b", '\b'
          "\\f", '\f'
          "\\n", '\n'
          "\\r", '\r'
          "\\t", '\t' ]
        |> List.map (fun (toMatch, result) -> stringReturn toMatch result)
        |> choice

    let unicodeChar =
        let convertToChar (s: string) =
            System.Int32.Parse(s.Substring(2), System.Globalization.NumberStyles.HexNumber) |> char

        regex @"\\u\d{4}" |>> convertToChar

    let ochar = choice [ unescapedChar; escapedChar; unicodeChar ]

    pchar '"' >>. manyChars ochar

let quotedString = quotedStringOpen .>> pchar '"'

let ident = identifier (IdentifierOptions())

let dotIdent = sepBy1 ident (pchar '.') |>> String.concat "."

let runParser parser  source  =
    match run parser source with
    | Success(result, _, _) -> Some result
    | Failure _ -> None

let parseOpenDirective =
    spaces >>. pchar '#' >>. (pstring "open" <|> pstring "o") >>. notFollowedBy ident
    >>. many (spaces >>. (dotIdent <|> quotedString))
    .>> spaces .>> eof
    |> runParser

let mkParseSingleArg names =
    spaces >>. pchar '#' >>. (names |> Seq.map pstring |> choice) >>. notFollowedBy ident
    >>. spaces >>. (dotIdent <|> quotedString)
    .>> spaces .>> eof
    |> runParser

let parseHelpDirective = mkParseSingleArg ["help"; "h"]
let parseHtypeDirective = mkParseSingleArg ["htype"]
let parseSaveDirective = mkParseSingleArg ["save"]
let parseRestoreDirective = mkParseSingleArg ["restore"]
