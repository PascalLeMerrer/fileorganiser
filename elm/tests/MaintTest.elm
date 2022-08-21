module MaintTest exposing (suite)

import Expect
import Main exposing (File, FileStatus(..), Model, defaultModel, filterDestinationDirectories)
import Test exposing (Test, describe, test)
import Time exposing (millisToPosix)


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


allDirs : List File
allDirs =
    [ dir1
    , dir2
    , dir3
    , dir4
    , dir5
    ]


model : Model
model =
    { defaultModel | destinationSubdirectories = [] }


suite : Test
suite =
    describe "filterDestinationDirectories"
        [ test "identifies filenames containing a given string" <|
            \_ ->
                let
                    filteredModel : Model
                    filteredModel =
                        { model
                            | destinationDirectoryFilter = "dirn"
                            , destinationSubdirectories = allDirs
                        }
                            |> filterDestinationDirectories

                    expected : List File
                    expected =
                        [ { dir1 | satisfiesFilter = True }
                        , dir2
                        , dir3
                        , { dir4 | satisfiesFilter = True }
                        , dir5
                        ]
                in
                Expect.equal expected filteredModel.destinationSubdirectories
        , test "identifies parent path containing a given string" <|
            \_ ->
                let
                    filteredModel : Model
                    filteredModel =
                        { model
                            | destinationDirectoryFilter = "ext"
                            , destinationSubdirectories = allDirs
                        }
                            |> filterDestinationDirectories

                    expected : List File
                    expected =
                        [ dir1
                        , dir2
                        , dir3
                        , { dir4 | satisfiesFilter = True }
                        , { dir5 | satisfiesFilter = True }
                        ]
                in
                Expect.equal expected filteredModel.destinationSubdirectories
        ]
