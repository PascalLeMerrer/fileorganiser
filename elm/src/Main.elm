port module Main exposing (..)

import Browser
import Browser.Dom
import Filesize
import Html exposing (Html, button, div, footer, form, h2, header, input, span, text)
import Html.Attributes exposing (class, disabled, id, placeholder, tabindex, type_, value)
import Html.Events as Events exposing (onClick, onFocus, onInput, onSubmit)
import Iso8601
import Json.Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import List.Extra
import String.Mark as Mark exposing (defaultOptions)
import Task
import Time exposing (Month(..), Posix, millisToPosix)



-- antislash is doubled for escaping


windowsPathSep =
    "\\"


unixPathSep =
    "/"



{- Maps Golang File -}


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


type Operation
    = Move
    | Rename
    | Delete


type FocusedZone
    = Confirmation
    | ErrorMessage
    | Filtering
    | LeftSide
    | FileNameEditor
    | DirNameEditor
    | RightSide
    | SourceSearchReplace


type alias Command =
    { operation : Operation
    , files : List File -- the files / dirs affected by the operation
    , destination : Maybe String -- destination dir if any FIXME this is a bad design; try to create specific commands instead
    , source : Maybe String -- source dir if any FIXME this is a bad design; try to create specific commands instead
    }



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- OUTPUT PORTS


port createDirectory : String -> Cmd msg


port getCurrentDirectoryPath : () -> Cmd msg


port getDestinationDirectoryFiles : String -> Cmd msg


port getSourceDirectoryContent : String -> Cmd msg


port getDestinationSubdirectories : String -> Cmd msg


port moveFiles : ( List String, String ) -> Cmd msg


port openFile : String -> Cmd msg


port removeFile : Json.Encode.Value -> Cmd msg


port renameFiles : List Json.Encode.Value -> Cmd msg


port selectDestinationDirectory : String -> Cmd msg


port selectSourceDirectory : String -> Cmd msg



-- INPUT PORTS


port filesRenamed : (Json.Encode.Value -> msg) -> Sub msg


port fileRemoved : (Json.Encode.Value -> msg) -> Sub msg


port receiveCreatedDirectory : (Json.Encode.Value -> msg) -> Sub msg


port receiveCurrentDirectoryPath : (String -> msg) -> Sub msg


port receiveDestinationDirectoryFiles : (Json.Encode.Value -> msg) -> Sub msg


port receiveError : (String -> msg) -> Sub msg


port receiveMovedFiles : (Json.Encode.Value -> msg) -> Sub msg


port receiveSelectedDestinationDirectory : (String -> msg) -> Sub msg


port receiveSelectedSourceDirectory : (String -> msg) -> Sub msg


port receiveSourceDirectoryContent : (Json.Encode.Value -> msg) -> Sub msg


port receiveSubDirectories : (Json.Encode.Value -> msg) -> Sub msg



-- MODEL


type alias Model =
    { areFilterSynchronized : Bool
    , destinationDirectoryFiles : List File
    , destinationDirectoryPath : String
    , destinationFilter : String
    , destinationSubdirectories : List File
    , editedFile : Maybe File
    , editedDirName : String
    , editedFileName : String
    , error : Maybe String
    , filesToDelete : List File
    , focusedZone : FocusedZone
    , history : List (List Command)
    , isCreatingDirectory : Bool
    , isUndoing : Bool
    , previousFocusedZone : FocusedZone
    , pathSeparator : String
    , sourceDirectoryFiles : List File
    , sourceDirectoryPath : String
    , sourceFilter : String
    , sourceReplace : String
    , sourceSearch : String
    , sourceSubDirectories : List File
    , timezone : Time.Zone
    }


defaultModel : Model
defaultModel =
    { areFilterSynchronized = False
    , destinationDirectoryFiles = []
    , destinationDirectoryPath = ""
    , destinationFilter = ""
    , destinationSubdirectories = []
    , editedDirName = ""
    , editedFile = Nothing
    , editedFileName = ""
    , error = Nothing
    , filesToDelete = []
    , focusedZone = LeftSide
    , history = []
    , isCreatingDirectory = False
    , isUndoing = False
    , pathSeparator = unixPathSep
    , previousFocusedZone = LeftSide
    , sourceDirectoryFiles = []
    , sourceDirectoryPath = "."
    , sourceFilter = ""
    , sourceReplace = ""
    , sourceSearch = ""
    , sourceSubDirectories = []
    , timezone = Time.utc
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , getCurrentDirectoryPath ()
        ]
    )



-- UPDATE


type alias Renaming =
    { file : File
    , originalPath : String
    }


type Msg
    = AdjustTimeZone Time.Zone
    | BackendReturnedCreatedDirectory File
    | BackendReturnedCurrentDirPath String
    | BackendReturnedDestinationDirectories (List File)
    | BackendReturnedDestinationDirectoryPath String
    | BackendReturnedDestinationFiles (List File)
    | BackendReturnedError String
    | BackendReturnedMovedFiles (List File)
    | BackendReturnedRemovedFile File String
    | BackendReturnedRenamedFiles (List File) (List String)
    | BackendReturnedSourceDirectoryContent (List File)
    | BackendReturnedSourceDirectoryPath String
    | NoOp
    | UserChangedDestinationFilter String
    | UserChangedSourceFilter String
    | UserChangedSourceReplace String
    | UserChangedSourceSearch String
    | UserClickedCancel
    | UserClickedClearSourceFilter
    | UserClickedClearDestinationFilter
    | UserClickedCloseError
    | UserClickedDelete
    | UserClickedDestinationDirectory File
    | UserClickedDestinationDirectoryButton
    | UserClickedDestinationFile File
    | UserClickedReload Target
    | UserClickedReplaceButton
    | UserClickedSourceDirectory File
    | UserClickedSourceDirectoryButton
    | UserClickedSourceFile File
    | UserClickedSynchronizeButton
    | UserChangedFocusedZone FocusedZone
    | UserModifiedFileName String
    | UserModifiedDirName String
    | UserPressedKey Target KeyboardEvent
    | UserSubmittedDirName
    | UserSubmittedFilename


type Target
    = Source
    | Destination


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        AdjustTimeZone newZone ->
            ( { model | timezone = newZone }
            , Cmd.none
            )

        BackendReturnedCreatedDirectory file ->
            ( { model
                | isCreatingDirectory = False
                , editedDirName = ""
                , focusedZone = RightSide
                , previousFocusedZone = model.focusedZone
              }
            , Cmd.batch
                -- TODO change to newly created dir?
                [ getDestinationSubdirectories model.destinationDirectoryPath
                , focusOn "container-right" NoOp
                ]
            )

        BackendReturnedCurrentDirPath path ->
            let
                pathSeparator =
                    if String.contains windowsPathSep path then
                        windowsPathSep

                    else
                        unixPathSep
            in
            ( { model
                | sourceDirectoryPath = path
                , destinationDirectoryPath = path
                , pathSeparator = pathSeparator
              }
            , Cmd.batch
                [ getSourceDirectoryContent path
                , getDestinationSubdirectories path
                , getDestinationDirectoryFiles path
                ]
            )

        BackendReturnedDestinationDirectories files ->
            ( { model | destinationSubdirectories = files }
                |> filterDestinationDirectories
            , Cmd.none
            )

        BackendReturnedError errorMsg ->
            ( { model
                | error = Just errorMsg
                , focusedZone = ErrorMessage
                , previousFocusedZone = model.focusedZone
              }
            , focusOn "close-error" NoOp
            )

        BackendReturnedSourceDirectoryContent directoryContent ->
            let
                ( dirList, fileList ) =
                    List.partition .isDir directoryContent
            in
            ( { model
                | sourceSubDirectories = dirList
                , sourceDirectoryFiles = fileList
              }
                |> filterSourceFiles
            , Cmd.none
            )

        BackendReturnedDestinationFiles fileList ->
            ( { model
                | destinationDirectoryFiles = fileList
              }
            , Cmd.none
            )

        BackendReturnedDestinationDirectoryPath path ->
            if path == "" then
                -- user canceled the selection
                ( model, Cmd.none )

            else
                ( { model | destinationDirectoryPath = path }
                , Cmd.batch
                    [ getDestinationSubdirectories path
                    , getDestinationDirectoryFiles path
                    ]
                )

        BackendReturnedMovedFiles files ->
            ( if model.isUndoing then
                -- don't add anything to history
                { model | isUndoing = False }

              else
                let
                    firstMovedFile =
                        List.head files

                    destination =
                        case firstMovedFile of
                            Just file ->
                                file.parentPath

                            Nothing ->
                                ""

                    source =
                        if destination == model.destinationDirectoryPath then
                            model.sourceDirectoryPath

                        else
                            model.destinationDirectoryPath
                in
                { model
                    | history =
                        [ { operation = Move
                          , files = files
                          , destination = Just destination
                          , source = Just source
                          }
                        ]
                            :: model.history
                }
            , Cmd.batch
                [ getSourceDirectoryContent model.sourceDirectoryPath
                , getDestinationDirectoryFiles model.destinationDirectoryPath
                ]
            )

        BackendReturnedRemovedFile _ _ ->
            -- TODO remove the unused params
            ( model
              -- TODO reload only the modified content if possible
            , Cmd.batch
                [ getSourceDirectoryContent model.sourceDirectoryPath
                , getDestinationDirectoryFiles model.destinationDirectoryPath
                ]
            )

        BackendReturnedRenamedFiles files originalPaths ->
            ( if model.isUndoing then
                -- don't add anything to history
                { model | isUndoing = False }

              else
                let
                    commands : List Command
                    commands =
                        List.map2
                            (\file originalPath ->
                                let
                                    newFile =
                                        { file | status = Selected }
                                in
                                { operation = Rename
                                , files = [ newFile ]
                                , destination = Just (newFile.parentPath ++ model.pathSeparator ++ newFile.name)
                                , source = Just originalPath
                                }
                            )
                            files
                            originalPaths
                in
                { model
                    | history = commands :: model.history
                    , editedFileName = ""
                    , editedFile = Nothing
                    , focusedZone = LeftSide
                    , previousFocusedZone = model.focusedZone
                    , sourceDirectoryFiles =
                        List.map
                            (\f ->
                                if f.status == Edited then
                                    { f | status = Selected }

                                else
                                    f
                            )
                            model.sourceDirectoryFiles
                }
            , Cmd.batch
                [ getSourceDirectoryContent model.sourceDirectoryPath
                , focusOn "container-left" NoOp
                ]
            )

        BackendReturnedSourceDirectoryPath path ->
            if path == "" then
                -- user canceled the selection
                ( model, Cmd.none )

            else
                ( { model | sourceDirectoryPath = path }
                , getSourceDirectoryContent path
                )

        UserPressedKey target event ->
            if model.isUndoing then
                -- ignore key presses while an undoing is being performed
                -- it could mess with the history
                ( model, Cmd.none )

            else
                processKeyboardShortcut model target event

        NoOp ->
            ( model, Cmd.none )

        UserChangedSourceFilter filteringString ->
            ( { model | sourceFilter = filteringString }
                |> filterSourceFiles
                |> synchronizeFilters
            , Cmd.none
            )

        UserChangedDestinationFilter filteringString ->
            ( { model | destinationFilter = filteringString }
                |> filterDestinationDirectories
            , Cmd.none
            )

        UserClickedClearDestinationFilter ->
            ( { model | destinationFilter = "" } |> filterDestinationDirectories
            , Cmd.none
            )

        UserClickedClearSourceFilter ->
            ( { model | sourceFilter = "" }
                |> filterSourceFiles
                |> synchronizeFilters
            , Cmd.none
            )

        UserClickedSourceDirectoryButton ->
            ( model, selectSourceDirectory model.sourceDirectoryPath )

        UserClickedDestinationDirectoryButton ->
            ( model, selectDestinationDirectory model.destinationDirectoryPath )

        UserClickedSourceFile file ->
            let
                newFile =
                    file |> toggleSelectionStatus

                updatedSourceFiles =
                    List.Extra.updateIf ((==) file) (\_ -> newFile) model.sourceDirectoryFiles
            in
            ( { model
                | sourceDirectoryFiles = updatedSourceFiles
                , focusedZone = LeftSide
                , previousFocusedZone = model.focusedZone
              }
                |> filterSourceFiles
            , focusOn "container-left" NoOp
            )

        UserClickedDestinationFile file ->
            let
                newFile =
                    file |> toggleSelectionStatus

                updatedDestinationFiles =
                    List.Extra.updateIf ((==) file) (\_ -> newFile) model.destinationDirectoryFiles
            in
            ( { model | destinationDirectoryFiles = updatedDestinationFiles }
            , Cmd.none
            )

        UserModifiedFileName newName ->
            ( { model | editedFileName = newName }, Cmd.none )

        UserModifiedDirName newName ->
            ( { model | editedDirName = newName }, Cmd.none )

        UserClickedDestinationDirectory file ->
            let
                newDestinationPath =
                    case file.name of
                        ".." ->
                            parentDir model model.destinationDirectoryPath

                        _ ->
                            model.destinationDirectoryPath ++ model.pathSeparator ++ file.name
            in
            ( { model | destinationDirectoryPath = newDestinationPath }
            , Cmd.batch
                [ getDestinationDirectoryFiles newDestinationPath
                , getDestinationSubdirectories newDestinationPath
                ]
            )

        UserClickedSourceDirectory file ->
            let
                newSourcePath =
                    case file.name of
                        ".." ->
                            file.parentPath

                        _ ->
                            file.parentPath ++ model.pathSeparator ++ file.name
            in
            ( { model | sourceDirectoryPath = newSourcePath }
            , getSourceDirectoryContent newSourcePath
            )

        UserChangedFocusedZone focus ->
            ( { model
                | focusedZone = focus
                , previousFocusedZone = model.focusedZone
              }
            , Cmd.none
            )

        UserClickedDelete ->
            removeSelectedFiles model

        UserClickedCancel ->
            let
                unselectForDeletion file =
                    if file.status == SelectedForDeletion then
                        { file | status = Selected }

                    else
                        file
            in
            ( { model
                | filesToDelete = []
                , focusedZone = LeftSide
                , previousFocusedZone = model.focusedZone
                , sourceDirectoryFiles = List.map unselectForDeletion model.sourceDirectoryFiles
                , destinationDirectoryFiles = List.map unselectForDeletion model.destinationDirectoryFiles
              }
            , focusOn "container-left" NoOp
            )

        UserClickedCloseError ->
            { model
                | error = Nothing
            }
                |> restoreFocus

        UserClickedReload target ->
            reload model target

        UserClickedSynchronizeButton ->
            ( { model | areFilterSynchronized = not model.areFilterSynchronized }
                |> synchronizeFilters
            , Cmd.none
            )

        UserChangedSourceReplace replaceString ->
            ( { model | sourceReplace = replaceString }
            , Cmd.none
            )

        UserChangedSourceSearch searchString ->
            ( { model | sourceSearch = searchString }, Cmd.none )

        UserClickedReplaceButton ->
            let
                renamings =
                    model.sourceDirectoryFiles
                        |> List.filterMap (nameReplacement model.sourceSearch model.sourceReplace)
            in
            ( { model | sourceReplace = "" }
            , applyRenaming model renamings
            )

        UserSubmittedDirName ->
            if model.isCreatingDirectory && model.editedDirName /= "" then
                createNewDirectory model

            else
                ( model, Cmd.none )

        UserSubmittedFilename ->
            let
                isConflicting =
                    List.any (\f -> f.name == model.editedFileName) model.sourceDirectoryFiles

                isNameEmpty =
                    String.isEmpty model.editedFileName
            in
            case ( model.editedFile, isConflicting, isNameEmpty ) of
                ( Just file, False, False ) ->
                    let
                        renaming =
                            { file = { file | name = model.editedFileName }
                            , originalPath = file.name
                            }
                    in
                    ( { model | error = Nothing }
                    , applyRenaming model [ renaming ]
                    )

                ( Just _, True, False ) ->
                    ( { model
                        | error = Just ("A file with the name " ++ model.editedFileName ++ " already exists in the source directory")
                        , focusedZone = ErrorMessage
                      }
                    , focusOn "close-error" NoOp
                    )

                _ ->
                    ( model, Cmd.none )


createNewDirectory : Model -> ( Model, Cmd Msg )
createNewDirectory model =
    let
        dirPath =
            model.destinationDirectoryPath
                ++ model.pathSeparator
                ++ model.editedDirName
    in
    ( model, createDirectory dirPath )


toggleSelectionStatus : File -> File
toggleSelectionStatus file =
    case file.status of
        Selected ->
            { file | status = Unselected }

        Unselected ->
            { file | status = Selected }

        Edited ->
            { file | status = Selected }

        SelectedForDeletion ->
            -- TODO  Remove from the list of files selected for deletion
            { file | status = Selected }


nameReplacement : String -> String -> File -> Maybe Renaming
nameReplacement before after file =
    if file.satisfiesFilter && String.contains before file.name then
        Just
            { file = { file | name = String.replace before after file.name }

            -- TODO prepend with the source dir?
            , originalPath = file.name
            }

    else
        Nothing


parentDir : Model -> String -> String
parentDir model path =
    let
        index =
            String.indexes model.pathSeparator path
                |> List.Extra.last
                |> Maybe.withDefault (String.length path)
    in
    String.slice 0 index path


filterSourceFiles : Model -> Model
filterSourceFiles model =
    let
        words =
            String.words model.sourceFilter
    in
    case words of
        [] ->
            { model
                | sourceDirectoryFiles = List.map (\f -> { f | satisfiesFilter = True }) model.sourceDirectoryFiles
            }

        _ ->
            { model
                | sourceDirectoryFiles =
                    List.map (\f -> { f | satisfiesFilter = filterByName words f }) model.sourceDirectoryFiles
            }


filterDestinationDirectories : Model -> Model
filterDestinationDirectories model =
    let
        words =
            String.words model.destinationFilter
    in
    case words of
        [] ->
            { model
                | destinationSubdirectories =
                    List.map (\f -> { f | satisfiesFilter = True })
                        model.destinationSubdirectories
            }

        _ ->
            { model
                | destinationSubdirectories =
                    List.map (\f -> { f | satisfiesFilter = filterByName words f || filterByParentPath words f })
                        model.destinationSubdirectories
            }


filterByName : List String -> File -> Bool
filterByName filters file =
    if file.name == ".." then
        True

    else
        let
            lowerCaseFilename =
                String.toLower file.name
        in
        List.all (\word -> String.contains (String.toLower word) lowerCaseFilename) filters


filterByParentPath : List String -> File -> Bool
filterByParentPath filters file =
    if file.name == ".." then
        True

    else
        let
            lowerCaseParentPath =
                String.toLower file.parentPath
        in
        List.all (\word -> String.contains (String.toLower word) lowerCaseParentPath) filters


synchronizeFilters : Model -> Model
synchronizeFilters model =
    if model.areFilterSynchronized then
        { model | destinationFilter = model.sourceFilter }
            |> filterDestinationDirectories

    else
        model


moveSelectedFiles : Model -> ( Model, Cmd Msg )
moveSelectedFiles model =
    let
        ( filesToMove, destination ) =
            case model.focusedZone of
                RightSide ->
                    ( model.destinationDirectoryFiles
                        |> List.filter (\f -> f.status == Selected)
                        |> List.map (\file -> file.parentPath ++ model.pathSeparator ++ file.name)
                    , model.sourceDirectoryPath
                    )

                _ ->
                    ( model.sourceDirectoryFiles
                        |> filterSelectedFiles
                        |> List.map (\file -> file.parentPath ++ model.pathSeparator ++ file.name)
                    , model.destinationDirectoryPath
                    )
    in
    ( model
    , moveFiles ( filesToMove, destination )
    )



-- TODO allow renaming destination files


renameSelectedSourceFile : Model -> ( Model, Cmd Msg )
renameSelectedSourceFile model =
    let
        fileToEdit =
            model.sourceDirectoryFiles
                |> List.Extra.find (\f -> f.satisfiesFilter && f.status == Selected)
    in
    case fileToEdit of
        Just file ->
            let
                sourceDirectoryFiles =
                    model.sourceDirectoryFiles
                        |> List.Extra.updateIf (\f -> f == file) (\f -> { f | status = Edited })
            in
            ( { model
                | editedFile = Just file
                , editedFileName = file.name
                , sourceDirectoryFiles = sourceDirectoryFiles
              }
            , focusOn "filename-input" NoOp
            )

        Nothing ->
            ( model, Cmd.none )


{-| Sets focus on an HTML element, then sends a msg when done (even if the element is not found)
-}
focusOn : String -> msg -> Cmd msg
focusOn elementId msg =
    Browser.Dom.focus elementId |> Task.attempt (\_ -> msg)


restoreFocus : Model -> ( Model, Cmd Msg )
restoreFocus model =
    ( { model | focusedZone = model.previousFocusedZone }
    , focusOn
        (case model.previousFocusedZone of
            Confirmation ->
                "delete-button"

            ErrorMessage ->
                "close-error"

            Filtering ->
                -- TODO handle filtering right
                "filtering-left"

            LeftSide ->
                "container-left"

            FileNameEditor ->
                "filename-input"

            DirNameEditor ->
                "dirname-input"

            RightSide ->
                "container-right"

            SourceSearchReplace ->
                "search-left"
        )
        NoOp
    )


prepareSelectedFilesForRemoval : Model -> ( Model, Cmd Msg )
prepareSelectedFilesForRemoval model =
    case model.focusedZone of
        LeftSide ->
            ( { model
                | filesToDelete = filterSelectedFiles model.sourceDirectoryFiles
                , focusedZone = Confirmation
                , previousFocusedZone = model.focusedZone
              }
                |> changeStatusOfSelectedSourceFiles SelectedForDeletion
            , focusOn "delete-button" NoOp
            )

        RightSide ->
            ( { model
                | filesToDelete =
                    model.destinationDirectoryFiles
                        |> List.filter (\f -> f.status == Selected)
                , focusedZone = Confirmation
                , previousFocusedZone = model.focusedZone
              }
                |> changeStatusOfSelectedDestinationFiles SelectedForDeletion
            , focusOn "delete-button" NoOp
            )

        _ ->
            ( model, Cmd.none )


filterSelectedFiles : List File -> List File
filterSelectedFiles files =
    List.filter (\f -> f.satisfiesFilter && f.status == Selected) files


changeStatusOfSelectedSourceFiles : FileStatus -> Model -> Model
changeStatusOfSelectedSourceFiles fileStatus model =
    { model
        | sourceDirectoryFiles =
            List.map
                (\file ->
                    if file.satisfiesFilter && file.status == Selected then
                        { file | status = fileStatus }

                    else
                        file
                )
                model.sourceDirectoryFiles
    }
        |> filterSourceFiles


changeStatusOfSelectedDestinationFiles : FileStatus -> Model -> Model
changeStatusOfSelectedDestinationFiles fileStatus model =
    { model
        | destinationDirectoryFiles =
            List.map
                (\file ->
                    if file.status == Selected then
                        { file | status = fileStatus }

                    else
                        file
                )
                model.destinationDirectoryFiles
    }


removeSelectedFiles : Model -> ( Model, Cmd Msg )
removeSelectedFiles model =
    let
        commands =
            model.filesToDelete
                |> List.map
                    (\file ->
                        removeFile <|
                            Json.Encode.string <|
                                file.parentPath
                                    ++ model.pathSeparator
                                    ++ file.name
                    )
    in
    ( { model
        | filesToDelete = []
        , focusedZone = LeftSide
      }
    , Cmd.batch
        (focusOn "container-left" NoOp :: commands)
    )


applyRenaming : Model -> List Renaming -> Cmd Msg
applyRenaming model renamings =
    let
        encodedRenamings =
            renamings
                |> List.map
                    (\renaming ->
                        Json.Encode.object
                            [ ( "oldName"
                              , renaming.file.parentPath
                                    ++ model.pathSeparator
                                    ++ renaming.originalPath
                                    |> Json.Encode.string
                              )
                            , ( "newName"
                              , renaming.file.parentPath
                                    ++ model.pathSeparator
                                    ++ renaming.file.name
                                    |> Json.Encode.string
                              )
                            ]
                    )
    in
    renameFiles encodedRenamings


undo : Model -> ( Model, Cmd Msg )
undo model =
    case List.head model.history of
        Just commands ->
            let
                ( updatedModel, cmds ) =
                    List.foldl
                        (\command ( modelAcc, cmdAcc ) ->
                            case command.operation of
                                Move ->
                                    undoMove modelAcc cmdAcc command

                                Rename ->
                                    undoRenaming modelAcc cmdAcc command

                                Delete ->
                                    Debug.todo "Implement delete cancellation"
                        )
                        ( model, [] )
                        commands
            in
            ( { updatedModel
                | history = List.drop 1 model.history
              }
            , Cmd.batch cmds
            )

        Nothing ->
            ( model, Cmd.none )


undoMove : Model -> List (Cmd Msg) -> Command -> ( Model, List (Cmd Msg) )
undoMove model cmds command =
    let
        source =
            command.destination
                |> Maybe.withDefault ""

        filesToMove =
            command.files
                |> List.map (\f -> source ++ model.pathSeparator ++ f.name)

        destination =
            command.source
                |> Maybe.withDefault ""
    in
    ( { model | isUndoing = True }
    , moveFiles ( filesToMove, destination ) :: cmds
    )


undoRenaming : Model -> List (Cmd Msg) -> Command -> ( Model, List (Cmd Msg) )
undoRenaming model cmds command =
    let
        oldName =
            command.destination |> Maybe.withDefault ""

        newName =
            command.source |> Maybe.withDefault ""

        encodedValue =
            Json.Encode.object
                [ ( "oldName", Json.Encode.string oldName )
                , ( "newName", Json.Encode.string newName )
                ]
    in
    ( { model
        | isUndoing = True
      }
    , renameFiles [ encodedValue ] :: cmds
    )


changeAllFileStatus : Model -> Target -> FileStatus -> ( Model, Cmd Msg )
changeAllFileStatus model target status =
    case target of
        Source ->
            let
                updatedFiles =
                    model.sourceDirectoryFiles
                        |> List.map
                            (\f ->
                                if f.satisfiesFilter then
                                    { f | status = status }

                                else
                                    f
                            )
            in
            ( { model
                | sourceDirectoryFiles = updatedFiles
              }
                |> filterSourceFiles
            , Cmd.none
            )

        Destination ->
            let
                updatedFiles =
                    model.destinationDirectoryFiles
                        |> List.map
                            (\f -> { f | status = status })
            in
            ( { model
                | destinationDirectoryFiles = updatedFiles
              }
            , Cmd.none
            )


reload : Model -> Target -> ( Model, Cmd Msg )
reload model target =
    case target of
        Source ->
            ( model
            , getSourceDirectoryContent model.sourceDirectoryPath
            )

        Destination ->
            ( model
            , Cmd.batch
                [ getDestinationSubdirectories model.destinationDirectoryPath
                , getDestinationDirectoryFiles model.destinationDirectoryPath
                ]
            )


openSelectedFile : Model -> Target -> ( Model, Cmd Msg )
openSelectedFile model target =
    let
        fileToOpen =
            case target of
                Source ->
                    model.sourceDirectoryFiles
                        |> List.Extra.find (\f -> f.satisfiesFilter && f.status == Selected)

                Destination ->
                    model.destinationDirectoryFiles
                        |> List.Extra.find (\f -> f.status == Selected)
    in
    case fileToOpen of
        Just file ->
            ( model, openFile <| file.parentPath ++ model.pathSeparator ++ file.name )

        Nothing ->
            ( model, Cmd.none )


openSelectedFolder : Model -> Target -> ( Model, Cmd Msg )
openSelectedFolder model target =
    let
        folderToOpen =
            case target of
                Source ->
                    model.sourceDirectoryPath

                Destination ->
                    model.destinationDirectoryPath
    in
    ( model, openFile folderToOpen )


showDirNameEditor : Model -> Target -> ( Model, Cmd Msg )
showDirNameEditor model target =
    case target of
        Source ->
            ( model, Cmd.none )

        Destination ->
            ( { model
                | isCreatingDirectory = True
              }
            , focusOn "dirname-input" NoOp
            )


processKeyboardShortcut : Model -> Target -> KeyboardEvent -> ( Model, Cmd Msg )
processKeyboardShortcut model target event =
    case model.focusedZone of
        Confirmation ->
            processConfirmationShortcuts model event

        LeftSide ->
            processMainShortcuts model target event

        RightSide ->
            processMainShortcuts model target event

        DirNameEditor ->
            case event.keyCode of
                Key.Escape ->
                    { model | isCreatingDirectory = False, editedDirName = "" }
                        |> restoreFocus

                _ ->
                    ( model, Cmd.none )

        FileNameEditor ->
            case event.keyCode of
                Key.Escape ->
                    let
                        sourceDirectoryFiles =
                            model.sourceDirectoryFiles
                                |> List.Extra.updateIf (\f -> f.status == Edited) (\f -> { f | status = Selected })
                    in
                    { model
                        | editedFile = Nothing
                        , editedFileName = ""
                        , sourceDirectoryFiles = sourceDirectoryFiles
                    }
                        |> restoreFocus

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


processConfirmationShortcuts : Model -> KeyboardEvent -> ( Model, Cmd Msg )
processConfirmationShortcuts model event =
    case ( event.keyCode, event.ctrlKey, event.metaKey ) of
        ( Key.Enter, False, False ) ->
            removeSelectedFiles model

        ( Key.Escape, False, False ) ->
            ( { model | filesToDelete = [], focusedZone = LeftSide }
            , focusOn "container-left" NoOp
            )

        _ ->
            ( model, Cmd.none )


processMainShortcuts : Model -> Target -> KeyboardEvent -> ( Model, Cmd Msg )
processMainShortcuts model target event =
    if event.ctrlKey || event.metaKey then
        case ( event.keyCode, event.shiftKey ) of
            ( Key.A, False ) ->
                changeAllFileStatus model target Selected

            ( Key.A, True ) ->
                changeAllFileStatus model target Unselected

            ( Key.Backspace, False ) ->
                prepareSelectedFilesForRemoval model

            ( Key.R, False ) ->
                reload model target

            ( Key.Z, False ) ->
                undo model

            _ ->
                ( model, Cmd.none )

    else
        case ( event.keyCode, event.shiftKey ) of
            ( Key.F2, False ) ->
                renameSelectedSourceFile model

            ( Key.F5, False ) ->
                reload model target

            ( Key.F, False ) ->
                openSelectedFolder model target

            ( Key.M, False ) ->
                moveSelectedFiles model

            ( Key.N, False ) ->
                showDirNameEditor model target

            ( Key.O, False ) ->
                openSelectedFile model target

            ( Key.R, False ) ->
                renameSelectedSourceFile model

            ( Key.Delete, False ) ->
                prepareSelectedFilesForRemoval model

            ( Key.U, False ) ->
                undo model

            _ ->
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fileRemoved (decodeFileWithPreviousName BackendReturnedRemovedFile)
        , filesRenamed (decodeRenamingList BackendReturnedRenamedFiles)
        , receiveCreatedDirectory (decodeFile BackendReturnedCreatedDirectory)
        , receiveCurrentDirectoryPath BackendReturnedCurrentDirPath
        , receiveSourceDirectoryContent (decodeFileList BackendReturnedSourceDirectoryContent)
        , receiveDestinationDirectoryFiles (decodeFileList BackendReturnedDestinationFiles)
        , receiveError BackendReturnedError
        , receiveMovedFiles (decodeFileList BackendReturnedMovedFiles)
        , receiveSelectedDestinationDirectory BackendReturnedDestinationDirectoryPath
        , receiveSelectedSourceDirectory BackendReturnedSourceDirectoryPath
        , receiveSubDirectories (decodeFileList BackendReturnedDestinationDirectories)
        ]


decodeRenamingList : (List File -> List String -> Msg) -> Json.Encode.Value -> Msg
decodeRenamingList msg value =
    let
        decodedFiles : Result Json.Decode.Error (List File)
        decodedFiles =
            Json.Decode.decodeValue (list fileDecoder) value

        decodedOriginalPaths : Result Json.Decode.Error (List String)
        decodedOriginalPaths =
            Json.Decode.decodeValue (list <| Json.Decode.field "PreviousName" Json.Decode.string) value
    in
    case ( decodedFiles, decodedOriginalPaths ) of
        ( Ok fileList, Ok originalPaths ) ->
            msg fileList originalPaths

        ( Err error, _ ) ->
            BackendReturnedError (Json.Decode.errorToString error)

        ( _, Err error ) ->
            BackendReturnedError (Json.Decode.errorToString error)


decodeFileList : (List File -> Msg) -> Json.Encode.Value -> Msg
decodeFileList msg value =
    let
        decodedFiles : Result Json.Decode.Error (List File)
        decodedFiles =
            Json.Decode.decodeValue (list fileDecoder) value
    in
    case decodedFiles of
        Ok fileList ->
            msg fileList

        Err error ->
            BackendReturnedError (Json.Decode.errorToString error)


decodeFileWithPreviousName : (File -> String -> Msg) -> Json.Encode.Value -> Msg
decodeFileWithPreviousName msg value =
    let
        decodedFile =
            Json.Decode.decodeValue fileDecoder value

        decodedPreviousName =
            Json.Decode.decodeValue (Json.Decode.field "PreviousName" Json.Decode.string) value
    in
    case ( decodedFile, decodedPreviousName ) of
        ( Ok file, Ok previousName ) ->
            msg file previousName

        ( Ok file, Err _ ) ->
            msg file ""

        ( Err error, _ ) ->
            BackendReturnedError (Json.Decode.errorToString error)


decodeFile : (File -> Msg) -> Json.Encode.Value -> Msg
decodeFile msg value =
    let
        decodedFile =
            Json.Decode.decodeValue fileDecoder value
    in
    case decodedFile of
        Ok file ->
            msg file

        Err error ->
            BackendReturnedError (Json.Decode.errorToString error)



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "app", id "app" ]
        [ viewHeader model
        , viewLeftSide model
        , viewRightSide model
        , viewFooter model
        ]


viewHeader : Model -> Html Msg
viewHeader model =
    header [ tabindex 0 ]
        [ div
            [ class "input-box"
            ]
            [ input
                [ class "input"
                , type_ "text"
                , id "filtering-left"
                , onInput UserChangedSourceFilter
                , onFocus (UserChangedFocusedZone Filtering)
                , value model.sourceFilter
                , placeholder "Enter one or more words to filter source files"
                ]
                []
            , button [ class "btn", onClick UserClickedClearSourceFilter ] [ text "Clear" ]
            ]
        , div []
            [ button [ class "btn link", onClick UserClickedSynchronizeButton ]
                [ if model.areFilterSynchronized then
                    text "Unlink"

                  else
                    text "Link"
                ]
            ]
        , div
            [ class "input-box"
            ]
            [ input
                [ class "input"
                , type_ "text"
                , id "filtering-right"
                , onInput UserChangedDestinationFilter
                , onFocus (UserChangedFocusedZone Filtering)
                , value model.destinationFilter
                , placeholder "Enter one or more words to filter destination directories"
                , disabled model.areFilterSynchronized
                ]
                []
            , button
                [ class "btn"
                , onClick UserClickedClearDestinationFilter
                , disabled model.areFilterSynchronized
                ]
                [ text "Clear" ]
            ]
        ]


viewLeftSide : Model -> Html Msg
viewLeftSide model =
    let
        conditionalAttributes =
            case ( model.editedFile, model.focusedZone ) of
                ( Nothing, LeftSide ) ->
                    [ Events.preventDefaultOn "keydown" (keyDecoderPreventingDefault Source)
                    ]

                ( Nothing, _ ) ->
                    [ onFocus (UserChangedFocusedZone LeftSide)
                    ]

                _ ->
                    []
    in
    div
        ([ id "container-left"
         , tabindex 1
         ]
            ++ conditionalAttributes
        )
    <|
        viewSource model


keyDecoderPreventingDefault : Target -> Json.Decode.Decoder ( Msg, Bool )
keyDecoderPreventingDefault target =
    decodeKeyboardEvent
        |> Json.Decode.map
            (\key ->
                ( UserPressedKey target key, True )
            )


simpleKeyDecoder : Target -> Json.Decode.Decoder Msg
simpleKeyDecoder target =
    decodeKeyboardEvent
        |> Json.Decode.map
            (\key ->
                UserPressedKey target key
            )


viewRightSide : Model -> Html Msg
viewRightSide model =
    let
        conditionalAttributes =
            case model.focusedZone of
                RightSide ->
                    [ Events.preventDefaultOn "keydown" (keyDecoderPreventingDefault Destination) ]

                _ ->
                    []
    in
    div
        ([ id "container-right"
         , tabindex 2
         , onFocus (UserChangedFocusedZone RightSide)
         ]
            ++ conditionalAttributes
        )
    <|
        viewDestination model


viewSource : Model -> List (Html Msg)
viewSource model =
    [ viewSourceSubdirectories model
    , viewSourceFiles model
    ]


viewDestination : Model -> List (Html Msg)
viewDestination model =
    [ viewDestinationSubdirectories model
    , viewDestinationFiles model
    ]


viewSourceSubdirectories : Model -> Html Msg
viewSourceSubdirectories model =
    div [ class "panel" ]
        [ div [ class <| "panel-header" ++ additionalHeaderClass model LeftSide ]
            [ h2 [] [ text <| "Source: " ++ truncatePath model.sourceDirectoryPath ]
            , span []
                [ button
                    [ class "btn"
                    , onClick (UserClickedReload Source)
                    ]
                    [ text "Refresh" ]
                , button
                    [ class "btn"
                    , onClick UserClickedSourceDirectoryButton
                    ]
                    [ text "..." ]
                ]
            ]
        , div
            [ class "panel-content" ]
            (model.sourceSubDirectories
                |> List.sortBy (.name >> String.toLower)
                |> List.map (viewDirectory model UserClickedSourceDirectory)
            )
        ]


additionalHeaderClass model zone =
    if model.focusedZone == zone then
        " focused"

    else
        " unfocused"


viewEditedDirectoryName : Model -> Html Msg
viewEditedDirectoryName model =
    form [ onSubmit UserSubmittedDirName ]
        [ input
            [ class "file-input"
            , id "dirname-input"
            , onInput UserModifiedDirName
            , onFocus (UserChangedFocusedZone DirNameEditor)
            , Events.on "keydown" (simpleKeyDecoder Destination)
            , value model.editedDirName
            ]
            []
        ]


viewDestinationSubdirectories : Model -> Html Msg
viewDestinationSubdirectories model =
    let
        newDirEditor =
            if model.isCreatingDirectory then
                [ viewEditedDirectoryName model ]

            else
                []
    in
    div [ class "panel" ]
        [ div [ class <| "panel-header" ++ additionalHeaderClass model RightSide ]
            [ h2 [] [ text <| "Destination: " ++ truncatePath model.destinationDirectoryPath ]
            , span []
                [ button
                    [ class "btn"
                    , onClick (UserClickedReload Destination)
                    ]
                    [ text "Refresh" ]
                , button
                    [ class "btn"
                    , onClick UserClickedDestinationDirectoryButton
                    ]
                    [ text "..." ]
                ]
            ]
        , div
            [ class "panel-content" ]
            (newDirEditor
                ++ (model.destinationSubdirectories
                        |> List.filter .satisfiesFilter
                        |> List.sortBy (.name >> String.toLower)
                        |> List.map (viewDirectory model UserClickedDestinationDirectory)
                   )
            )
        ]


viewDirectory : Model -> (File -> Msg) -> File -> Html Msg
viewDirectory _ onClickMsg file =
    div
        [ class "file dir"
        , onClick (onClickMsg file)
        ]
        [ text <| file.name ]


viewSourceFiles : Model -> Html Msg
viewSourceFiles model =
    let
        count =
            List.length model.sourceDirectoryFiles |> String.fromInt
    in
    div [ class "panel" ]
        [ div [ class <| "panel-header" ++ additionalHeaderClass model LeftSide ]
            [ h2 [] [ text <| count ++ " files in source directory" ]
            , div [ class "search-form" ]
                [ input
                    [ class "input"
                    , id "search-left"
                    , onFocus (UserChangedFocusedZone SourceSearchReplace)
                    , onInput UserChangedSourceSearch
                    , placeholder "Search"
                    , type_ "text"
                    , value model.sourceSearch
                    ]
                    []
                , input
                    [ class "input"
                    , onFocus (UserChangedFocusedZone SourceSearchReplace)
                    , onInput UserChangedSourceReplace
                    , placeholder "Replace with"
                    , type_ "text"
                    , value model.sourceReplace
                    ]
                    []
                , button
                    [ class "btn"
                    , onClick UserClickedReplaceButton
                    ]
                    [ text "Replace" ]
                ]
            ]
        , div
            [ class "panel-content" ]
            (model.sourceDirectoryFiles
                |> List.filter .satisfiesFilter
                |> List.sortBy (.name >> String.toLower)
                |> List.map (viewFile model UserClickedSourceFile True)
            )
        ]


viewDestinationFiles : Model -> Html Msg
viewDestinationFiles model =
    let
        count =
            List.length model.destinationDirectoryFiles |> String.fromInt
    in
    div [ class "panel" ]
        [ div [ class <| "panel-header" ++ additionalHeaderClass model RightSide ]
            [ h2 [] [ text <| count ++ " files in destination directory" ]
            ]
        , div
            [ class "panel-content" ]
            (model.destinationDirectoryFiles
                |> List.sortBy (.name >> String.toLower)
                |> List.map (viewFile model UserClickedDestinationFile False)
            )
        ]


viewFile : Model -> (File -> Msg) -> Bool -> File -> Html Msg
viewFile model onClickMsg canBeSearchedAndReplaced file =
    if file.status == Edited then
        viewEditedFilename model

    else
        viewReadOnlyFile model onClickMsg canBeSearchedAndReplaced file


highlight : String -> String -> List (Html Msg)
highlight =
    Mark.markWith { defaultOptions | minTermLength = 1 }


viewReadOnlyFile : Model -> (File -> Msg) -> Bool -> File -> Html Msg
viewReadOnlyFile model onClickMsg canBeSearchedAndReplaced file =
    let
        className =
            case file.status of
                Selected ->
                    "filename selected"

                Unselected ->
                    "filename"

                Edited ->
                    "filename"

                SelectedForDeletion ->
                    "filename marked-for-deletion"

        fileName =
            if canBeSearchedAndReplaced then
                case ( model.sourceSearch, model.sourceReplace ) of
                    ( "", _ ) ->
                        [ text file.name ]

                    ( searchedString, "" ) ->
                        highlight searchedString file.name

                    ( searchedString, replacementString ) ->
                        file.name
                            |> String.replace searchedString replacementString
                            |> highlight replacementString

            else
                [ text file.name ]
    in
    div
        [ class "file"
        , onClick (onClickMsg file)
        ]
        [ div [ class className ] fileName
        , div [] [ text <| Filesize.format file.size ]
        , div [ class "filemodificationdate" ] [ viewDate model file.modTime ]
        ]


viewEditedFilename : Model -> Html Msg
viewEditedFilename model =
    form [ onSubmit UserSubmittedFilename ]
        [ input
            [ class "file-input"
            , id "filename-input"
            , onInput UserModifiedFileName
            , onFocus (UserChangedFocusedZone FileNameEditor)
            , Events.on "keydown" (simpleKeyDecoder Source)
            , value model.editedFileName
            ]
            []
        ]


viewDate : Model -> Time.Posix -> Html Msg
viewDate model time =
    let
        monthToString timeMonth =
            case timeMonth of
                Jan ->
                    "01"

                Feb ->
                    "02"

                Mar ->
                    "03"

                Apr ->
                    "04"

                May ->
                    "05"

                Jun ->
                    "06"

                Jul ->
                    "07"

                Aug ->
                    "08"

                Sep ->
                    "09"

                Oct ->
                    "10"

                Nov ->
                    "11"

                Dec ->
                    "12"

        day =
            String.fromInt (Time.toDay model.timezone time)
                |> String.padLeft 2 '0'

        month =
            Time.toMonth model.timezone time
                |> monthToString

        year =
            String.fromInt (Time.toYear model.timezone time)
    in
    text (day ++ "/" ++ month ++ "/" ++ year)


viewFooter : Model -> Html Msg
viewFooter model =
    let
        isWaitingForConfirmation =
            List.length model.filesToDelete > 0

        className =
            if model.error /= Nothing || isWaitingForConfirmation then
                "danger"

            else
                ""

        conditionalAttributes =
            if isWaitingForConfirmation then
                [ Events.preventDefaultOn "keydown" (keyDecoderPreventingDefault Source)
                , onFocus (UserChangedFocusedZone Confirmation)
                ]

            else
                []
    in
    footer
        ([ class className ]
            ++ conditionalAttributes
        )
        [ case model.error of
            Nothing ->
                viewFocusedZone model

            Just errorMsg ->
                div []
                    [ text errorMsg
                    , button [ id "close-error", class "btn", onClick UserClickedCloseError ] [ text "Ok" ]
                    ]
        , case model.filesToDelete of
            [] ->
                text ""

            _ ->
                span
                    []
                    [ text "This will permanently delete the selected files. This cannot be undone."
                    , button [ class "btn", onClick UserClickedCancel ] [ text "Cancel" ]
                    , button [ class "btn", onClick UserClickedDelete, id "delete-button" ] [ text "DELETE" ]
                    ]
        ]


viewFocusedZone : Model -> Html Msg
viewFocusedZone model =
    text <|
        case model.focusedZone of
            Confirmation ->
                "Confirmation | "

            Filtering ->
                "Filtering | "

            LeftSide ->
                "LeftSide | "

            FileNameEditor ->
                "NameEditor | "

            RightSide ->
                "RightSide | "

            SourceSearchReplace ->
                "SourceSearchReplace | "

            DirNameEditor ->
                "DirNameEditor | "

            ErrorMessage ->
                "ErrorMessage"


maxVisiblePathLength =
    45


truncatePath : String -> String
truncatePath fullPath =
    let
        actualLength =
            String.length fullPath
    in
    if actualLength > maxVisiblePathLength then
        "..." ++ String.dropLeft (actualLength - maxVisiblePathLength) fullPath

    else
        fullPath
