// For more information see https://aka.ms/fsharp-console-apps
module Main
open Parser

let input = "// this is a comment
1234 5678
ababababa1 69420! <> 3
// a comment at the end shouldnt do anything either"

lex input |> printfn "%A"