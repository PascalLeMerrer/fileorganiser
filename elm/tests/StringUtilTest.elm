module StringUtilTest exposing (..)

import Expect
import StringUtil exposing (Chunk)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "StringUtil"
        [ test "Does not split a word" <|
            \_ ->
                "auniqueword"
                    |> StringUtil.split
                    |> Expect.equal [ word "auniqueword" ]
        , test "Splits words separated by spaces" <|
            \_ ->
                "words separated by spaces"
                    |> StringUtil.split
                    |> Expect.equal
                        [ word "words"
                        , separator " "
                        , word "separated"
                        , separator " "
                        , word "by"
                        , separator " "
                        , word "spaces"
                        ]
        , test "Splits words separated by dashes" <|
            \_ ->
                "words-separated-by-dashes"
                    |> StringUtil.split
                    |> Expect.equal
                        [ word "words"
                        , separator "-"
                        , word "separated"
                        , separator "-"
                        , word "by"
                        , separator "-"
                        , word "dashes"
                        ]
        , test "Splits words separated by underscores" <|
            \_ ->
                "words_separated_by_underscores"
                    |> StringUtil.split
                    |> Expect.equal
                        [ word "words"
                        , separator "_"
                        , word "separated"
                        , separator "_"
                        , word "by"
                        , separator "_"
                        , word "underscores"
                        ]
        , test "Splits words separated by dots" <|
            \_ ->
                "words.separated.by.dots"
                    |> StringUtil.split
                    |> Expect.equal
                        [ word "words"
                        , separator "."
                        , word "separated"
                        , separator "."
                        , word "by"
                        , separator "."
                        , word "dots"
                        ]
        , test "Splits words separated by parens" <|
            \_ ->
                "words(separatedBy)parens"
                    |> StringUtil.split
                    |> Expect.equal
                        [ word "words"
                        , separator "("
                        , word "separatedBy"
                        , separator ")"
                        , word "parens"
                        ]
        , test "Splits words separated by brackets" <|
            \_ ->
                "words[separatedBy]brackets"
                    |> StringUtil.split
                    |> Expect.equal
                        [ word "words"
                        , separator "["
                        , word "separatedBy"
                        , separator "]"
                        , word "brackets"
                        ]
        , test "Splits words separated by various separators" <|
            \_ ->
                "a.lot of_words-separated[by](various)separators"
                    |> StringUtil.split
                    |> Expect.equal
                        [ word "a"
                        , separator "."
                        , word "lot"
                        , separator " "
                        , word "of"
                        , separator "_"
                        , word "words"
                        , separator "-"
                        , word "separated"
                        , separator "["
                        , word "by"
                        , separator "]"
                        , separator "("
                        , word "various"
                        , separator ")"
                        , word "separators"
                        ]
        ]


word : String -> Chunk
word string =
    { isSeparator = False
    , value = string
    }


separator : String -> Chunk
separator string =
    { isSeparator = True
    , value = string
    }
