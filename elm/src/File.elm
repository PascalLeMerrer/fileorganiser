module File exposing (File, FileStatus(..), defaultDir, extendSelectionToNext, extendSelectionToPrevious, fileDecoder, selectNext, selectPrevious, selectSimilar, toggleSelectionStatus, withName, withParentPath, withStatus)

import Iso8601
import JaroWinkler
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (hardcoded, required)
import List.Extra
import Time exposing (Posix, millisToPosix)


type alias File =
    { isDir : Bool
    , mode : Int
    , modTime : Posix
    , name : String
    , parentPath : String
    , satisfiesFilter : Bool
    , size : Int
    , status : FileStatus
    }


type FileStatus
    = Unselected
    | Edited
    | Selected
    | SelectedForDeletion


defaultDir : File
defaultDir =
    { isDir = True
    , mode = 777
    , modTime = millisToPosix 0
    , name = ""
    , parentPath = ""
    , satisfiesFilter = False
    , size = 0
    , status = Unselected
    }


extendSelectionToNext : List File -> List File
extendSelectionToNext files =
    let
        ( isAtLeastOneFileSelected, updatedFiles ) =
            extendSelection List.Extra.mapAccuml files
    in
    if isAtLeastOneFileSelected then
        updatedFiles

    else
        updatedFiles
            |> selectLast


extendSelectionToPrevious : List File -> List File
extendSelectionToPrevious files =
    let
        ( isAtLeastOneFileSelected, updatedFiles ) =
            extendSelection List.Extra.mapAccumr files
    in
    if isAtLeastOneFileSelected then
        updatedFiles

    else
        updatedFiles
            |> selectLast


fileDecoder : Decoder File
fileDecoder =
    Json.Decode.succeed File
        |> required "IsDir" Json.Decode.bool
        |> required "Mode" Json.Decode.int
        |> required "ModTime" Iso8601.decoder
        |> required "Name" Json.Decode.string
        |> required "DirPath" Json.Decode.string
        |> hardcoded False
        |> required "Size" Json.Decode.int
        |> hardcoded Unselected


selectNext : List File -> List File
selectNext files =
    let
        ( isAtLeastOneFileSelected, updatedFiles ) =
            selectNextOrPrevious List.Extra.mapAccuml files
    in
    if isAtLeastOneFileSelected then
        updatedFiles

    else
        updatedFiles
            |> selectLast


selectPrevious : List File -> List File
selectPrevious files =
    let
        ( isAtLeastOneFileSelected, updatedFiles ) =
            selectNextOrPrevious List.Extra.mapAccumr files
    in
    if isAtLeastOneFileSelected then
        updatedFiles

    else
        updatedFiles
            |> selectFirst



{- selects files whose name have a high level of similarity -}


selectSimilar : Float -> List File -> List File
selectSimilar level files =
    let
        firstSelectedFile : Maybe File
        firstSelectedFile =
            List.Extra.find (\f -> f.status == Selected) files

        similarityLevel =
            level / 10
    in
    case firstSelectedFile of
        Just file ->
            List.map (selectIfSimilar file.name similarityLevel) files

        Nothing ->
            files


toggleSelectionStatus : File -> File
toggleSelectionStatus file =
    case file.status of
        Unselected ->
            { file | status = Selected }

        Edited ->
            { file | status = Selected }

        Selected ->
            { file | status = Unselected }

        SelectedForDeletion ->
            -- TODO  Remove from the list of files selected for deletion?
            { file | status = Selected }


withName : String -> File -> File
withName name file =
    { file | name = name }


withParentPath : String -> File -> File
withParentPath path file =
    { file | parentPath = path }


withStatus : FileStatus -> File -> File
withStatus fileStatus file =
    { file | status = fileStatus }


extendSelection :
    ((SelectionAccumulator -> File -> ( SelectionAccumulator, File )) -> SelectionAccumulator -> List File -> ( SelectionAccumulator, List File ))
    -> List File
    -> ( Bool, List File )
extendSelection visit files =
    let
        ( finalAccumulator, updatedFiles ) =
            visit
                (\acc file ->
                    let
                        newAcc : SelectionAccumulator
                        newAcc =
                            { acc | isPreviousSelected = file.status == Selected }
                    in
                    if acc.isPreviousSelected then
                        ( { newAcc | selectedCount = acc.selectedCount + 1 }
                        , { file | status = Selected }
                        )

                    else
                        ( newAcc, file )
                )
                initialAccumulator
                files

        initialAccumulator : SelectionAccumulator
        initialAccumulator =
            { isPreviousSelected = False
            , selectedCount = 0
            }

        isAtLeastOneFileSelected : Bool
        isAtLeastOneFileSelected =
            finalAccumulator.selectedCount > 0
    in
    ( isAtLeastOneFileSelected, updatedFiles )


selectFirst : List File -> List File
selectFirst files =
    List.Extra.updateAt 0 (\f -> { f | status = Selected }) files


selectIfSimilar : String -> Float -> File -> File
selectIfSimilar referenceName level file =
    if JaroWinkler.similarity referenceName file.name >= level then
        { file | status = Selected }

    else
        { file | status = Unselected }


selectLast : List File -> List File
selectLast files =
    List.Extra.updateAt (List.length files - 1) (\f -> { f | status = Selected }) files


selectNextOrPrevious :
    ((SelectionAccumulator -> File -> ( SelectionAccumulator, File )) -> SelectionAccumulator -> List File -> ( SelectionAccumulator, List File ))
    -> List File
    -> ( Bool, List File )
selectNextOrPrevious visit files =
    let
        ( finalAccumulator, updatedFiles ) =
            visit
                (\acc file ->
                    let
                        newAcc : SelectionAccumulator
                        newAcc =
                            { acc | isPreviousSelected = file.status == Selected }
                    in
                    if acc.isPreviousSelected then
                        ( { newAcc | selectedCount = acc.selectedCount + 1 }
                        , { file | status = Selected }
                        )

                    else
                        ( newAcc, { file | status = Unselected } )
                )
                initialAccumulator
                files

        initialAccumulator : SelectionAccumulator
        initialAccumulator =
            { isPreviousSelected = False
            , selectedCount = 0
            }

        isAtLeastOneFileSelected : Bool
        isAtLeastOneFileSelected =
            finalAccumulator.selectedCount > 0
    in
    ( isAtLeastOneFileSelected, updatedFiles )


type alias SelectionAccumulator =
    { isPreviousSelected : Bool
    , selectedCount : Int
    }
