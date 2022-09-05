module Pattern exposing (..)

import Regex exposing (Regex)


type alias Pattern =
    List Token


type Token
    = RawString String
    | Joker


escapeSpecialChars : String -> String
escapeSpecialChars string =
    string
        |> String.replace "." "\\."
        |> String.replace "[" "\\["
        |> String.replace "]" "\\]"
        |> String.replace "(" "\\("
        |> String.replace ")" "\\)"


fromString : String -> List Token
fromString string =
    string
        |> String.split "*"
        |> List.map escapeSpecialChars
        |> List.map RawString
        |> List.intersperse Joker
        |> List.filter (\t -> t /= RawString "")


toRegexp : Pattern -> Maybe Regex
toRegexp pattern =
    pattern
        |> List.map
            (\token ->
                case token of
                    RawString string ->
                        "(" ++ string ++ ")"

                    Joker ->
                        "(.+)"
            )
        |> String.concat
        |> Regex.fromString
