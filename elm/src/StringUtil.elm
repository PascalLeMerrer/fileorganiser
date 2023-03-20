module StringUtil exposing (..)

import Char exposing (isAlphaNum)
import Parser exposing ((|.), (|=), Parser, Step(..), chompIf, chompWhile, getChompedString, loop, map, oneOf, run, succeed, symbol)


type alias Chunk =
    { isSeparator : Bool
    , value : String
    }


split : String -> List Chunk
split string =
    case run chunksParser string of
        Ok chunks ->
            chunks

        Err _ ->
            [ { isSeparator = False
              , value = string
              }
            ]


chunksParser : Parser (List Chunk)
chunksParser =
    loop [] chunkParser


chunkParser : List Chunk -> Parser (Step (List Chunk) (List Chunk))
chunkParser reversedChunks =
    oneOf
        [ succeed (\_ -> Loop ({ isSeparator = True, value = "(" } :: reversedChunks)) |= symbol "("
        , succeed (\_ -> Loop ({ isSeparator = True, value = ")" } :: reversedChunks)) |= symbol ")"
        , succeed (\_ -> Loop ({ isSeparator = True, value = "[" } :: reversedChunks)) |= symbol "["
        , succeed (\_ -> Loop ({ isSeparator = True, value = "]" } :: reversedChunks)) |= symbol "]"
        , succeed (\_ -> Loop ({ isSeparator = True, value = " " } :: reversedChunks)) |= symbol " "
        , succeed (\_ -> Loop ({ isSeparator = True, value = "." } :: reversedChunks)) |= symbol "."
        , succeed (\_ -> Loop ({ isSeparator = True, value = "-" } :: reversedChunks)) |= symbol "-"
        , succeed (\_ -> Loop ({ isSeparator = True, value = "_" } :: reversedChunks)) |= symbol "_"
        , succeed (\_ -> Loop ({ isSeparator = True, value = " " } :: reversedChunks)) |= symbol " "
        , succeed (\w -> Loop ({ isSeparator = False, value = w } :: reversedChunks)) |= word
        , succeed ()
            |> map (\_ -> Done (List.reverse reversedChunks))
        ]


isChar =
    \c -> not <| List.member c [ '(', ')', '[', ']', ' ', '.', '-', ' ', '_' ]


word : Parser String
word =
    getChompedString <|
        succeed ()
            |. chompIf isChar
            |. chompWhile isChar
