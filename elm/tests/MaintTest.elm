module MaintTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Main exposing (FileStatus(..), Model, defaultModel, filterDestinationDirectories)
import Test exposing (..)
import Time exposing (millisToPosix)


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


dir2 =
    { dir1 | name = "dir2" }


dir3 =
    { dir1 | name = "different" }


dir4 =
    { dir1
        | name = "dirname4"
        , parentPath = "/some/path/extended"
    }


dir5 =
    { dir1
        | name = "dir5"
        , parentPath = "/some/path/extended"
    }


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
                    filteredModel =
                        { model
                            | destinationFilter = "dirn"
                            , destinationSubdirectories = allDirs
                        }
                            |> filterDestinationDirectories

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
                    filteredModel =
                        { model
                            | destinationFilter = "ext"
                            , destinationSubdirectories = allDirs
                        }
                            |> filterDestinationDirectories

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
