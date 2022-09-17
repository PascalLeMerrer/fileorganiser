module FileTest exposing (suite)

import Expect
import File exposing (File, FileStatus(..), extendSelectionToNext, extendSelectionToPrevious, selectNext, selectPrevious, selectSimilar, withStatus)
import Fixtures exposing (filteredDir1, filteredDir2, filteredDir3, filteredDir4, filteredDir5, filteredDir6, filteredDir7, filteredDir8)
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
        , test "selectNext selects the last file in a list when none is selected" <|
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
        , test "selectPrevious selects the previous file in a list when there is one" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        selectPrevious
                            [ filteredDir1
                            , filteredDir2
                            , filteredDir3
                            , filteredDir4 |> withStatus Selected
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
        , test "selectPrevious does nothing when the currently selected file is the first" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        selectPrevious expected

                    expected : List File
                    expected =
                        [ filteredDir1 |> withStatus Selected
                        , filteredDir2
                        , filteredDir3
                        , filteredDir4
                        , filteredDir5
                        ]
                in
                Expect.equal expected actual
        , test "selectPrevious selects the first file in a list when none is selected" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        selectPrevious
                            [ filteredDir1
                            , filteredDir2
                            , filteredDir3
                            , filteredDir4
                            , filteredDir5
                            ]

                    expected : List File
                    expected =
                        [ filteredDir1 |> withStatus Selected
                        , filteredDir2
                        , filteredDir3
                        , filteredDir4
                        , filteredDir5
                        ]
                in
                Expect.equal expected actual
        , test "extendSelectionToNext selects the next file after the first selected" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        extendSelectionToNext
                            [ filteredDir1
                            , filteredDir2 |> withStatus Selected
                            , filteredDir3
                            , filteredDir4
                            , filteredDir5
                            ]

                    expected : List File
                    expected =
                        [ filteredDir1
                        , filteredDir2 |> withStatus Selected
                        , filteredDir3 |> withStatus Selected
                        , filteredDir4
                        , filteredDir5
                        ]
                in
                Expect.equal expected actual
        , test "extendSelectionToPrevious selects the file before the first selected" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        extendSelectionToPrevious
                            [ filteredDir1
                            , filteredDir2
                            , filteredDir3 |> withStatus Selected
                            , filteredDir4
                            , filteredDir5
                            ]

                    expected : List File
                    expected =
                        [ filteredDir1
                        , filteredDir2 |> withStatus Selected
                        , filteredDir3 |> withStatus Selected
                        , filteredDir4
                        , filteredDir5
                        ]
                in
                Expect.equal expected actual
        , test "selectSimilar selects the files with a name stringly looking like the given one" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        selectSimilar
                            0.8
                            [ filteredDir1
                            , filteredDir6 |> withStatus Selected
                            , filteredDir2
                            , filteredDir7
                            , filteredDir3
                            , filteredDir8
                            , filteredDir4 |> withStatus Selected
                            , filteredDir5
                            ]

                    expected : List File
                    expected =
                        [ filteredDir1
                        , filteredDir6 |> withStatus Selected
                        , filteredDir2
                        , filteredDir7 |> withStatus Selected
                        , filteredDir3
                        , filteredDir8 |> withStatus Selected
                        , filteredDir4
                        , filteredDir5
                        ]
                in
                Expect.equal expected actual
        ]
