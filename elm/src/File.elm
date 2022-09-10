module File exposing (File, FileStatus(..), defaultDir, fileDecoder, selectNext, selectPrevious, toggleSelectionStatus, withName, withParentPath, withStatus)

import Iso8601
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


type alias SelectionAccumulator =
    { isPreviousSelected : Bool
    , selectedCount : Int
    }


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


selectLast : List File -> List File
selectLast files =
    List.Extra.updateAt (List.length files - 1) (\f -> { f | status = Selected }) files


selectFirst : List File -> List File
selectFirst files =
    List.Extra.updateAt 0 (\f -> { f | status = Selected }) files


selectNext : List File -> List File
selectNext files =
    let
        ( finalAccumulator, updatedFiles ) =
            List.Extra.mapAccuml
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
    if isAtLeastOneFileSelected then
        updatedFiles

    else
        updatedFiles
            |> selectLast


selectPrevious : List File -> List File
selectPrevious files =
    let
        ( finalAccumulator, updatedFiles ) =
            List.Extra.mapAccumr
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
    if isAtLeastOneFileSelected then
        updatedFiles

    else
        updatedFiles
            |> selectFirst


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
