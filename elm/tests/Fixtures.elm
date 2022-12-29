module Fixtures exposing (allDirs, dir1, dir2, dir3, dir4, dir5, filteredDir1, filteredDir2, filteredDir3, filteredDir4, filteredDir5, filteredDir6, filteredDir7, filteredDir8, model, windowsDir)

import File exposing (File, FileStatus(..), withName)
import Main exposing (Model, defaultModel)
import Time exposing (millisToPosix)


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


filteredDir6 : File
filteredDir6 =
    filteredDir1 |> withName "a name with random chars 1 [123]"


filteredDir7 : File
filteredDir7 =
    filteredDir1 |> withName "a name with random chars 2 [456]"


filteredDir8 : File
filteredDir8 =
    filteredDir1 |> withName "a name with random chars 3 [678] - Foo"


model : Model
model =
    { defaultModel | destinationSubdirectories = [] }


windowsDir : File
windowsDir =
    { dir1
        | name = "windows dir"
        , parentPath = "C:\\some\\path\\extended"
    }


withSatisfiedFilter : File -> File
withSatisfiedFilter file =
    { file | satisfiesFilter = True }
