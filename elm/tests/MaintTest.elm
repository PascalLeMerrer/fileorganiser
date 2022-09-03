module MaintTest exposing (suite)

import Expect
import Main exposing (File, FileStatus(..), Model, defaultDir, defaultModel, filterDestinationDirectories, pathElements, select, truncateConcatenatedNames, withName, withParentPath, withStatus)
import Test exposing (Test, describe, test)
import Time exposing (millisToPosix)


suite : Test
suite =
    describe "filterDestinationDirectories"
        [ test "identifies filenames containing a given string" <|
            \_ ->
                let
                    expected : List File
                    expected =
                        [ { dir1 | satisfiesFilter = True }
                        , dir2
                        , dir3
                        , { dir4 | satisfiesFilter = True }
                        , dir5
                        ]

                    filteredModel : Model
                    filteredModel =
                        { model
                            | destinationDirectoryFilter = "dirn"
                            , destinationSubdirectories = allDirs
                        }
                            |> filterDestinationDirectories
                in
                Expect.equal expected filteredModel.destinationSubdirectories
        , test "identifies parent path containing a given string" <|
            \_ ->
                let
                    expected : List File
                    expected =
                        [ dir1
                        , dir2
                        , dir3
                        , { dir4 | satisfiesFilter = True }
                        , { dir5 | satisfiesFilter = True }
                        ]

                    filteredModel : Model
                    filteredModel =
                        { model
                            | destinationDirectoryFilter = "ext"
                            , destinationSubdirectories = allDirs
                        }
                            |> filterDestinationDirectories
                in
                Expect.equal expected filteredModel.destinationSubdirectories
        , test "pathElements returns the list of nested path and their names" <|
            \_ ->
                let
                    elements : List File
                    elements =
                        pathElements defaultModel [] <| dir5.parentPath ++ defaultModel.pathSeparator ++ dir5.name

                    expected : List File
                    expected =
                        [ defaultDir
                            |> withName "some"
                            |> withParentPath ""
                        , defaultDir
                            |> withName "path"
                            |> withParentPath "/some"
                        , defaultDir
                            |> withName "extended"
                            |> withParentPath "/some/path"
                        , defaultDir
                            |> withName "dir5"
                            |> withParentPath "/some/path/extended"
                        ]
                in
                Expect.equal expected elements
        , test "pathElements ignores ." <|
            \_ ->
                let
                    elements : List File
                    elements =
                        pathElements defaultModel [] "."

                    expected : List File
                    expected =
                        []
                in
                Expect.equal expected elements
        , test "truncate returns a list of files whose cumulated name length does not exceed given size" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        truncateConcatenatedNames 22 allDirs

                    expected : List File
                    expected =
                        [ dir3
                        , dir4
                        , dir5
                        ]
                in
                Expect.equal expected actual
        , test "select selects only the clicked file when neither CTRL or SHIFT are pressed" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        select model allDirs dir3

                    expected : List File
                    expected =
                        [ dir1
                        , dir2
                        , dir3 |> withStatus Selected
                        , dir4
                        , dir5
                        ]
                in
                Expect.equal expected actual
        , test "select unselects the clicked file when neither CTRL or SHIFT are pressed" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        select model
                            [ dir1
                            , dir2
                            , clickedFile
                            , dir4
                            , dir5
                            ]
                            dir3

                    clickedFile : File
                    clickedFile =
                        dir3 |> withStatus Selected

                    expected : List File
                    expected =
                        allDirs
                in
                Expect.equal expected actual
        , test "select adds the clicked file to the current selection if it is unselected and CTRL is pressed" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        select
                            { model | isControlPressed = True }
                            [ dir1
                            , dir2
                            , clickedFile
                            , dir4
                            , dir5
                            ]
                            dir4

                    clickedFile : File
                    clickedFile =
                        dir3 |> withStatus Selected

                    expected : List File
                    expected =
                        [ dir1
                        , dir2
                        , dir3 |> withStatus Selected
                        , dir4 |> withStatus Selected
                        , dir5
                        ]
                in
                Expect.equal expected actual
        , test "select removes the clicked file from the current selection if it is selected and CTRL is pressed" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        select
                            { model | isControlPressed = True }
                            [ dir1
                            , dir2
                            , dir3 |> withStatus Selected
                            , clickedFile
                            , dir5
                            ]
                            clickedFile

                    clickedFile : File
                    clickedFile =
                        dir4 |> withStatus Selected

                    expected : List File
                    expected =
                        [ dir1
                        , dir2
                        , dir3 |> withStatus Selected
                        , dir4
                        , dir5
                        ]
                in
                Expect.equal expected actual
        , test "select selects the file range from the first selected to the clicked file when it is after and SHIFT is pressed" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        select
                            { model | isShiftPressed = True }
                            [ filteredDir1
                            , filteredDir2 |> withStatus Selected
                            , filteredDir3
                            , filteredDir4
                            , filteredDir5
                            ]
                            filteredDir4

                    expected : List File
                    expected =
                        [ filteredDir1
                        , filteredDir2 |> withStatus Selected
                        , filteredDir3 |> withStatus Selected
                        , filteredDir4 |> withStatus Selected
                        , filteredDir5
                        ]
                in
                Expect.equal expected actual
        , test "select selects the file range from the clicked file to the last selected when it is before and SHIFT is pressed" <|
            \_ ->
                let
                    actual : List File
                    actual =
                        select
                            { model | isShiftPressed = True }
                            [ filteredDir1
                            , filteredDir2
                            , filteredDir3
                            , filteredDir4 |> withStatus Selected
                            , filteredDir5
                            ]
                            filteredDir2

                    expected : List File
                    expected =
                        [ filteredDir1
                        , filteredDir2 |> withStatus Selected
                        , filteredDir3 |> withStatus Selected
                        , filteredDir4 |> withStatus Selected
                        , filteredDir5
                        ]
                in
                Expect.equal expected actual
        ]


allDirs : List File
allDirs =
    [ dir1
    , dir2
    , dir3
    , dir4
    , dir5
    ]


dir1 : File
dir1 =
    { isDir = True
    , mode = 777
    , modTime = millisToPosix 0
    , name = "dirname"
    , parentPath = "/some/path/"
    , satisfiesFilter = False
    , size = 0
    , status = Unselected
    }


dir2 : File
dir2 =
    { dir1 | name = "dir2" }


dir3 : File
dir3 =
    { dir1 | name = "different" }


dir4 : File
dir4 =
    { dir1
        | name = "dirname4"
        , parentPath = "/some/path/extended"
    }


dir5 : File
dir5 =
    { dir1
        | name = "dir5"
        , parentPath = "/some/path/extended"
    }


filteredDir1 : File
filteredDir1 =
    dir1 |> withSatisfiedFilter


filteredDir2 : File
filteredDir2 =
    dir2 |> withSatisfiedFilter


filteredDir3 : File
filteredDir3 =
    dir3 |> withSatisfiedFilter


filteredDir4 : File
filteredDir4 =
    dir4 |> withSatisfiedFilter


filteredDir5 : File
filteredDir5 =
    dir5 |> withSatisfiedFilter


model : Model
model =
    { defaultModel | destinationSubdirectories = [] }


withSatisfiedFilter : File -> File
withSatisfiedFilter file =
    { file | satisfiesFilter = True }
