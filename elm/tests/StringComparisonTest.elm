module StringComparisonTest exposing (..)

import Expect exposing (Expectation)
import StringComparison exposing (isSimilarityLevelGreaterThan)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "isSimilarityLevelGreaterThan"
        [ test "returns true when comparing string with more than level common chars" <|
            \_ ->
                isSimilarityLevelGreaterThan "abcdeijk" "abcdefgh" 5
                    |> Expect.true "Expect the strings to be detected as similar"
        , test "detects similarity even if the first string is shorter than the second" <|
            \_ ->
                isSimilarityLevelGreaterThan "zbcde" "abcdefgh" 4
                    |> Expect.true "Expect the strings to be detected as similar"
        , test "detects similarity even if the first string is longer than the second" <|
            \_ ->
                isSimilarityLevelGreaterThan "zBCDExkHamsld" "aBCDEfgH" 5
                    |> Expect.true "Expect the strings to be detected as similar"
        ]
