module SearchReplaceTest exposing (suite)

import Expect
import Pattern exposing (Token(..), fromString)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "SearchReplace"
        [ describe "fromString"
            [ test "parses a string with a joker" <|
                \_ ->
                    let
                        actual : List Token
                        actual =
                            fromString "a*b"

                        expected : List Token
                        expected =
                            [ RawString "a", Joker, RawString "b" ]
                    in
                    Expect.equal expected actual
            , test "parses a string without any joker" <|
                \_ ->
                    let
                        actual : List Token
                        actual =
                            fromString "ab"

                        expected : List Token
                        expected =
                            [ RawString "ab" ]
                    in
                    Expect.equal expected actual
            , test "parses a string with several jokers" <|
                \_ ->
                    let
                        actual : List Token
                        actual =
                            fromString "ab*cd*"

                        expected : List Token
                        expected =
                            [ RawString "ab", Joker, RawString "cd", Joker ]
                    in
                    Expect.equal expected actual
            , test "parses a string with a leading joker" <|
                \_ ->
                    let
                        actual : List Token
                        actual =
                            fromString "*abcd"

                        expected : List Token
                        expected =
                            [ Joker, RawString "abcd" ]
                    in
                    Expect.equal expected actual
            , test "escapes special chars" <|
                \_ ->
                    let
                        actual : List Token
                        actual =
                            fromString ".[]()"

                        expected : List Token
                        expected =
                            --[  RawString "\\.\\[\\]\\(\\)" ]
                            [ RawString "\\.\\[\\]\\(\\)" ]
                    in
                    Expect.equal expected actual
            ]
        ]
