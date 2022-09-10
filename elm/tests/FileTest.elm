module FileTest exposing (suite)

import Expect
import File exposing (File, FileStatus(..), selectNext, withStatus)
import Fixtures exposing (filteredDir1, filteredDir2, filteredDir3, filteredDir4, filteredDir5)
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "File module"
        [ test "selectNext selects the next file" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        selectNext
                            [ filteredDir1
                            , filteredDir2 |> withStatus Selected
                            , filteredDir3
                            , filteredDir4
                            , filteredDir5
                            ]

                    expected : List File
                    expected =
                        [ filteredDir1
                        , filteredDir2
                        , filteredDir3 |> withStatus Selected
                        , filteredDir4
                        , filteredDir5
                        ]
                in
                Expect.equal expected actual
        , test "selectNext does nothing when the currently selected file is the last" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        selectNext expected

                    expected : List File
                    expected =
                        [ filteredDir1
                        , filteredDir2
                        , filteredDir3
                        , filteredDir4
                        , filteredDir5 |> withStatus Selected
                        ]
                in
                Expect.equal expected actual
        , test "selectLast selects the last file in a list" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        selectNext
                            [ filteredDir1
                            , filteredDir2
                            , filteredDir3
                            , filteredDir4
                            , filteredDir5
                            ]

                    expected : List File
                    expected =
                        [ filteredDir1
                        , filteredDir2
                        , filteredDir3
                        , filteredDir4
                        , filteredDir5 |> withStatus Selected
                        ]
                in
                Expect.equal expected actual
        ]
