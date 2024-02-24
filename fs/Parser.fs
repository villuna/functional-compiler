module Parser
open System

type Token = int * string

let splitBy (p: char -> bool) (s: string) : string*string =
    if String.length s = 0 then
        ("", "")
    else
        let mutable i = 0
        let length = String.length s

        while i < length && p s.[i] do
            i <- i + 1

        (s.Substring(0, i), s.Substring(i))

let isSpace (c: char) : bool =
    Char.IsWhiteSpace(c)

let operators = ["=="; "<>"; ">="; "<="; "->"]

let rec lex_h (line_num: int) (input: string) : list<Token> =
    if String.length input = 0 then
        []
    else
        let fst = input.[0]
        if Char.IsWhiteSpace(fst) then
            let line =  if fst = '\n' then
                            line_num + 1
                        else 
                            line_num
            lex_h line (input.Substring(1))
        else if Char.IsAsciiDigit(fst) then
            let (pre, post) = splitBy Char.IsAsciiDigit input
            (line_num, pre) :: lex_h line_num post
        else if Char.IsAsciiLetter(fst) then
            let (tail, rest) = splitBy (fun c -> Char.IsAsciiLetterOrDigit c || c = '_') input
            (line_num, $"{fst}{tail}") :: lex_h line_num rest
        else if input.StartsWith("//") then
            let (_, rest) = splitBy (fun c -> c <> '\n') input
            lex_h line_num rest
        else if List.exists (fun (op: string) -> (input.StartsWith(op))) operators then
            let len = List.find (fun (op: string) -> (input.StartsWith(op))) operators |> String.length
            (line_num, input.Substring(0, len)) :: lex_h line_num (input.Substring(len))
        else
            (line_num, input.Substring(0, 1)) :: lex_h line_num (input.Substring(1))

let lex = lex_h 0
