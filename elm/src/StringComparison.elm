module StringComparison exposing (..)

import List.Extra


isSimilarityLevelGreaterThan : String -> String -> Int -> Bool
isSimilarityLevelGreaterThan string1 string2 threshold =
    let
        len1 =
            String.length string1

        len2 =
            String.length string2

        chars1 =
            string1
                |> String.padRight (len2 - len1) ' '
                |> String.toList

        chars2 =
            string2
                |> String.padRight (len1 - len2) ' '
                |> String.toList

        charList =
            List.Extra.zip chars1 chars2

        similarCharCount =
            List.foldl compareChars 0 charList
    in
    similarCharCount >= threshold


compareChars : ( Char, Char ) -> Int -> Int
compareChars ( char1, char2 ) currentLevel =
    if char1 == char2 then
        currentLevel + 1

    else
        currentLevel



--compareChars : List Char -> List Char -> Int -> Int
--compareChars chars1 chars2 currentLevel =
--    if List.isEmpty chars1 || List.isEmpty chars2 then
--        currentLevel
--
--    else if List.head chars1 == List.head chars2 then
--        compareChars (List.drop 1 chars1) (List.drop 1 chars2) (currentLevel + 1)
--
--    else
--        compareChars (List.drop 1 chars1) (List.drop 1 chars2) currentLevel
