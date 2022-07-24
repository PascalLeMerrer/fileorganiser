port module Main exposing (..)

import Browser
import Filesize
import Html exposing (Html, button, div, footer, form, h2, header, input, span, text)
import Html.Attributes exposing (autofocus, class, disabled, id, placeholder, tabindex, type_, value)
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
import Time exposing (Month(..), Posix)



-- antislash is doubled for escaping


windowsPathSep =
    "\\"


unixPathSep =
    "/"



{- Maps Golang FileInfo -}


type alias FileInfo =
    { isDir : Bool
    , mode : Int
    , modTime : Posix
    , name : String
    , isSelected : Bool
    , size : Int
    }


type Operation
    = Move
    | Rename
    | Delete


type FocusedZone
    = Confirmation
    | Filtering
    | LeftSide
    | NameEditor
    | RightSide
    | SourceSearchReplace


type alias Command =
    { operation : Operation
    , files : List FileInfo -- the files / dirs affected by the operation
    , destination : Maybe String -- destination dir if any
    , source : Maybe String -- source dir if any
    }


fileInfoDecoder : Decoder FileInfo
fileInfoDecoder =
    Json.Decode.succeed FileInfo
        |> required "IsDir" Json.Decode.bool
        |> required "Mode" Json.Decode.int
        |> required "ModTime" Iso8601.decoder
        |> required "Name" Json.Decode.string
        |> hardcoded False
        |> required "Size" Json.Decode.int



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


port getCurrentDirectoryPath : () -> Cmd msg


port getDestinationDirectoryFiles : String -> Cmd msg


port getSourceDirectoryContent : String -> Cmd msg


port getDestinationSubdirectories : String -> Cmd msg


port moveFiles : ( List String, String ) -> Cmd msg


port openFile : String -> Cmd msg


port removeFile : Json.Encode.Value -> Cmd msg


port renameFile : Json.Encode.Value -> Cmd msg


port selectDestinationDirectory : String -> Cmd msg


port selectSourceDirectory : String -> Cmd msg



-- INPUT PORTS


port fileRenamed : (Json.Encode.Value -> msg) -> Sub msg


port fileRemoved : (Json.Encode.Value -> msg) -> Sub msg


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
    , destinationDirectoryFiles : List FileInfo
    , destinationDirectoryPath : String
    , destinationFilter : String
    , destinationSubdirectories : List FileInfo
    , editedFile : Maybe FileInfo
    , editedName : String
    , error : Maybe String
    , filesToDelete : List FileInfo
    , sourceFilter : String
    , sourceSearch : String
    , sourceReplace : String
    , filteredDestinationSubdirectories : List FileInfo
    , filteredSourceDirectoryFiles : List FileInfo
    , history : List Command
    , pathSeparator : String
    , focusedZone : FocusedZone
    , sourceDirectoryFiles : List FileInfo
    , sourceDirectoryPath : String
    , sourceSubDirectories : List FileInfo
    , timezone : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { areFilterSynchronized = False
      , destinationDirectoryFiles = []
      , destinationDirectoryPath = ""
      , destinationFilter = ""
      , destinationSubdirectories = []
      , editedFile = Nothing
      , editedName = ""
      , error = Nothing
      , filesToDelete = []
      , sourceFilter = ""
      , sourceSearch = ""
      , sourceReplace = ""
      , filteredDestinationSubdirectories = []
      , filteredSourceDirectoryFiles = []
      , history = []
      , pathSeparator = unixPathSep
      , focusedZone = LeftSide
      , sourceDirectoryFiles = []
      , sourceDirectoryPath = "."
      , sourceSubDirectories = []
      , timezone = Time.utc
      }
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , getCurrentDirectoryPath ()
        ]
    )



-- UPDATE


type Msg
    = AdjustTimeZone Time.Zone
    | BackendReturnedCurrentDirPath String
    | BackendReturnedDestinationDirectories (List FileInfo)
    | BackendReturnedDestinationDirectoryPath String
    | BackendReturnedDestinationFiles (List FileInfo)
    | BackendReturnedError String
    | BackendReturnedMovedFiles (List FileInfo)
    | BackendReturnedRemovedFile FileInfo String
    | BackendReturnedRenamedFile FileInfo String
    | BackendReturnedSourceDirectoryContent (List FileInfo)
    | BackendReturnedSourceDirectoryPath String
    | NoOp
    | UserChangedDestinationFilter String
    | UserChangedSourceFilter String
    | UserChangedSourceReplace String
    | UserChangedSourceSearch String
    | UserClickedCancel
    | UserClickedClearSourceFilter
    | UserClickedClearDestinationFilter
    | UserClickedDelete
    | UserClickedDestinationDirectory FileInfo
    | UserClickedDestinationDirectoryButton
    | UserClickedDestinationFile FileInfo
    | UserClickedReload Target
    | UserClickedReplaceButton
    | UserClickedSourceDirectory FileInfo
    | UserClickedSourceDirectoryButton
    | UserClickedSourceFile FileInfo
    | UserClickedSynchronizeButton
    | UserChangedFocusedZone FocusedZone
    | UserModifiedFileName String
    | UserPressedKey Target KeyboardEvent
    | UserValidatedFilename


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

        BackendReturnedDestinationDirectories fileInfos ->
            ( { model | destinationSubdirectories = fileInfos }
                |> filterDestinationDirectories
            , Cmd.none
            )

        BackendReturnedError errorMsg ->
            ( { model | error = Just errorMsg }, Cmd.none )

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

        BackendReturnedMovedFiles fileInfos ->
            let
                command : Command
                command =
                    { operation = Move
                    , files = fileInfos
                    , destination = Just model.destinationDirectoryPath
                    , source = Just model.sourceDirectoryPath
                    }
            in
            ( { model | history = command :: model.history }
            , Cmd.batch
                [ getSourceDirectoryContent model.sourceDirectoryPath
                , getDestinationDirectoryFiles model.destinationDirectoryPath
                ]
            )

        BackendReturnedRemovedFile _ _ ->
            -- TODO remove the unused params
            ( model, getSourceDirectoryContent model.sourceDirectoryPath )

        BackendReturnedRenamedFile fileInfo originalPath ->
            let
                newFileInfo =
                    { fileInfo | isSelected = True }

                newPath =
                    model.sourceDirectoryPath
                        ++ model.pathSeparator
                        ++ newFileInfo.name

                command : Command
                command =
                    { operation = Rename
                    , files = [ newFileInfo ]
                    , destination = Just newPath
                    , source = Just originalPath
                    }
            in
            case model.editedFile of
                Just editedFile ->
                    let
                        updatedSourceFiles =
                            List.Extra.updateIf (\f -> f == editedFile) (\_ -> newFileInfo) model.sourceDirectoryFiles
                    in
                    ( { model
                        | editedName = ""
                        , editedFile = Nothing
                        , history = command :: model.history
                        , sourceDirectoryFiles = updatedSourceFiles
                      }
                        |> filterSourceFiles
                    , Cmd.none
                    )

                Nothing ->
                    -- TODO update the modified file or reload all
                    ( { model
                        | history = command :: model.history
                      }
                    , getSourceDirectoryContent model.sourceDirectoryPath
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

        UserClickedSourceFile fileInfo ->
            let
                newFileInfo =
                    { fileInfo | isSelected = not fileInfo.isSelected }

                updatedSourceFiles =
                    List.Extra.updateIf ((==) fileInfo) (\_ -> newFileInfo) model.sourceDirectoryFiles
            in
            ( { model | sourceDirectoryFiles = updatedSourceFiles }
                |> filterSourceFiles
            , Cmd.none
            )

        UserClickedDestinationFile fileInfo ->
            let
                newFileInfo =
                    { fileInfo | isSelected = not fileInfo.isSelected }

                updatedDestinationFiles =
                    List.Extra.updateIf ((==) fileInfo) (\_ -> newFileInfo) model.destinationDirectoryFiles
            in
            ( { model | destinationDirectoryFiles = updatedDestinationFiles }
            , Cmd.none
            )

        UserModifiedFileName newName ->
            ( { model | editedName = newName }, Cmd.none )

        UserValidatedFilename ->
            case model.editedFile of
                Just fileInfo ->
                    ( model
                    , applyRenaming model fileInfo.name model.editedName
                    )

                Nothing ->
                    ( model, Cmd.none )

        UserClickedDestinationDirectory fileInfo ->
            let
                newDestinationPath =
                    case fileInfo.name of
                        ".." ->
                            parentDir model model.destinationDirectoryPath

                        _ ->
                            model.destinationDirectoryPath ++ model.pathSeparator ++ fileInfo.name
            in
            ( { model | destinationDirectoryPath = newDestinationPath }
            , Cmd.batch
                [ getDestinationDirectoryFiles newDestinationPath
                , getDestinationSubdirectories newDestinationPath
                ]
            )

        UserClickedSourceDirectory fileInfo ->
            let
                newSourcePath =
                    case fileInfo.name of
                        ".." ->
                            parentDir model model.sourceDirectoryPath

                        _ ->
                            model.sourceDirectoryPath ++ model.pathSeparator ++ fileInfo.name
            in
            ( { model | sourceDirectoryPath = newSourcePath }
            , getSourceDirectoryContent newSourcePath
            )

        UserChangedFocusedZone focus ->
            ( { model | focusedZone = focus }, Cmd.none )

        UserClickedDelete ->
            removeSelectedFiles model

        UserClickedCancel ->
            ( { model | filesToDelete = [], focusedZone = LeftSide }
            , Cmd.none
            )

        UserClickedReload target ->
            reload model target

        UserClickedSynchronizeButton ->
            ( { model | areFilterSynchronized = not model.areFilterSynchronized }
                |> synchronizeFilters
            , Cmd.none
            )

        UserChangedSourceReplace replaceString ->
            { model | sourceReplace = replaceString }
                |> replaceInSourceFilenames

        UserChangedSourceSearch searchString ->
            ( { model | sourceSearch = searchString }, Cmd.none )

        UserClickedReplaceButton ->
            let
                ( oldNames, newNames ) =
                    model
                        |> filterSourceFiles
                        |> .filteredSourceDirectoryFiles
                        |> List.filterMap (nameReplacement model.sourceSearch model.sourceReplace)
                        |> List.unzip
            in
            ( { model | sourceReplace = "" }
            , List.map2 (applyRenaming model) oldNames newNames
                |> Cmd.batch
            )


nameReplacement : String -> String -> FileInfo -> Maybe ( String, String )
nameReplacement before after fileInfo =
    if String.contains before fileInfo.name then
        Just
            ( fileInfo.name
            , String.replace before after fileInfo.name
            )

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
                | filteredSourceDirectoryFiles = model.sourceDirectoryFiles
            }

        _ ->
            { model
                | filteredSourceDirectoryFiles =
                    List.filter (filterByName words) model.sourceDirectoryFiles
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
                | filteredDestinationSubdirectories = model.destinationSubdirectories
            }

        _ ->
            { model
                | filteredDestinationSubdirectories =
                    List.filter (filterByName words) model.destinationSubdirectories
            }


filterByName : List String -> FileInfo -> Bool
filterByName filters fileInfo =
    if fileInfo.name == ".." then
        True

    else
        let
            lowerCaseFilename =
                String.toLower fileInfo.name
        in
        List.all (\word -> String.contains (String.toLower word) lowerCaseFilename) filters


synchronizeFilters : Model -> Model
synchronizeFilters model =
    if model.areFilterSynchronized then
        { model | destinationFilter = model.sourceFilter }
            |> filterDestinationDirectories

    else
        model


moveSelectedSourceFiles : Model -> ( Model, Cmd Msg )
moveSelectedSourceFiles model =
    let
        filesToMove : List String
        filesToMove =
            model.filteredSourceDirectoryFiles
                |> List.filter .isSelected
                |> List.map (\fileinfo -> model.sourceDirectoryPath ++ model.pathSeparator ++ fileinfo.name)
    in
    ( model
    , moveFiles ( filesToMove, model.destinationDirectoryPath )
    )


renameSelectedFile : Model -> ( Model, Cmd Msg )
renameSelectedFile model =
    let
        fileToEdit =
            model.filteredSourceDirectoryFiles
                |> List.Extra.find .isSelected
    in
    case fileToEdit of
        Just fileInfo ->
            ( { model | editedFile = Just fileInfo, editedName = fileInfo.name }
            , Cmd.none
            )

        Nothing ->
            ( model, Cmd.none )


prepareSelectedFilesForRemoval : Model -> ( Model, Cmd Msg )
prepareSelectedFilesForRemoval model =
    ( { model
        | filesToDelete =
            model.filteredSourceDirectoryFiles
                |> List.filter .isSelected
        , focusedZone = Confirmation
      }
    , Cmd.none
    )


removeSelectedFiles : Model -> ( Model, Cmd Msg )
removeSelectedFiles model =
    ( { model
        | filesToDelete = []
        , focusedZone = LeftSide
      }
    , model.filesToDelete
        |> List.map
            (\fileInfo ->
                removeFile <|
                    Json.Encode.string <|
                        model.sourceDirectoryPath
                            ++ model.pathSeparator
                            ++ fileInfo.name
            )
        |> Cmd.batch
    )


replaceInSourceFilenames : Model -> ( Model, Cmd Msg )
replaceInSourceFilenames model =
    ( { model
        | filteredSourceDirectoryFiles =
            List.map (replace model.sourceSearch model.sourceReplace) model.sourceDirectoryFiles
      }
    , Cmd.none
    )


replace : String -> String -> FileInfo -> FileInfo
replace before after fileInfo =
    { fileInfo
        | name = String.replace before after fileInfo.name
    }


applyRenaming : Model -> String -> String -> Cmd Msg
applyRenaming model oldName newName =
    let
        oldPath =
            model.sourceDirectoryPath ++ model.pathSeparator ++ oldName

        newPath =
            model.sourceDirectoryPath ++ model.pathSeparator ++ newName

        encodedValue =
            Json.Encode.object
                [ ( "oldName", Json.Encode.string oldPath )
                , ( "newName", Json.Encode.string newPath )
                ]
    in
    renameFile encodedValue


cancel : Model -> ( Model, Cmd Msg )
cancel model =
    let
        commandToCancel =
            model.history
                |> List.head
    in
    case commandToCancel of
        Just command ->
            case command.operation of
                Move ->
                    cancelMove command model

                Rename ->
                    cancelRenaming command model

                Delete ->
                    Debug.todo "Implement delete cancellation"

        Nothing ->
            ( model, Cmd.none )


cancelMove : Command -> Model -> ( Model, Cmd Msg )
cancelMove command model =
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
    ( model
    , moveFiles ( filesToMove, destination )
    )


cancelRenaming : Command -> Model -> ( Model, Cmd Msg )
cancelRenaming command model =
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
    ( { model | editedFile = List.head command.files }
    , renameFile encodedValue
    )


selectAllFiles : Model -> Target -> ( Model, Cmd Msg )
selectAllFiles model target =
    case target of
        Source ->
            let
                -- TODO should we select filtered files only?
                updatedFiles =
                    model.sourceDirectoryFiles
                        |> List.map (\f -> { f | isSelected = True })
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
                        |> List.map (\f -> { f | isSelected = True })
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
        ( fileToOpen, dirPath ) =
            case target of
                Source ->
                    ( model.filteredSourceDirectoryFiles
                        |> List.Extra.find .isSelected
                    , model.sourceDirectoryPath
                    )

                Destination ->
                    ( model.destinationDirectoryFiles
                        |> List.Extra.find .isSelected
                    , model.destinationDirectoryPath
                    )
    in
    case fileToOpen of
        Just fileInfo ->
            ( model, openFile <| Debug.log "path" <| dirPath ++ model.pathSeparator ++ fileInfo.name )

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


processKeyboardShortcut : Model -> Target -> KeyboardEvent -> ( Model, Cmd Msg )
processKeyboardShortcut model target event =
    case model.focusedZone of
        Confirmation ->
            processConfirmationShortcuts model event

        LeftSide ->
            processMainShortcuts model target event

        RightSide ->
            processMainShortcuts model target event

        _ ->
            ( model, Cmd.none )


processConfirmationShortcuts : Model -> KeyboardEvent -> ( Model, Cmd Msg )
processConfirmationShortcuts model event =
    case ( event.keyCode, event.ctrlKey, event.metaKey ) of
        ( Key.Enter, False, False ) ->
            removeSelectedFiles model

        ( Key.Escape, False, False ) ->
            ( { model | filesToDelete = [], focusedZone = LeftSide }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


processMainShortcuts : Model -> Target -> KeyboardEvent -> ( Model, Cmd Msg )
processMainShortcuts model target event =
    case ( event.keyCode, event.ctrlKey, event.metaKey ) of
        ( Key.F2, False, False ) ->
            renameSelectedFile model

        ( Key.F5, False, False ) ->
            reload model target

        ( Key.A, True, False ) ->
            selectAllFiles model target

        ( Key.A, False, True ) ->
            selectAllFiles model target

        ( Key.Backspace, False, True ) ->
            prepareSelectedFilesForRemoval model

        ( Key.F, False, False ) ->
            openSelectedFolder model target

        ( Key.M, False, False ) ->
            moveSelectedSourceFiles model

        ( Key.O, False, False ) ->
            openSelectedFile model target

        ( Key.R, False, False ) ->
            renameSelectedFile model

        ( Key.R, True, False ) ->
            reload model target

        ( Key.Delete, False, False ) ->
            prepareSelectedFilesForRemoval model

        ( Key.U, False, False ) ->
            cancel model

        ( Key.Z, True, False ) ->
            cancel model

        ( Key.Z, False, True ) ->
            cancel model

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ fileRemoved (decodeFileInfo BackendReturnedRemovedFile)
        , fileRenamed (decodeFileInfo BackendReturnedRenamedFile)
        , receiveCurrentDirectoryPath BackendReturnedCurrentDirPath
        , receiveSourceDirectoryContent (decodeFileInfoList BackendReturnedSourceDirectoryContent)
        , receiveDestinationDirectoryFiles (decodeFileInfoList BackendReturnedDestinationFiles)
        , receiveError BackendReturnedError
        , receiveMovedFiles (decodeFileInfoList BackendReturnedMovedFiles)
        , receiveSelectedDestinationDirectory BackendReturnedDestinationDirectoryPath
        , receiveSelectedSourceDirectory BackendReturnedSourceDirectoryPath
        , receiveSubDirectories (decodeFileInfoList BackendReturnedDestinationDirectories)
        ]


decodeFileInfoList : (List FileInfo -> Msg) -> Json.Encode.Value -> Msg
decodeFileInfoList msg value =
    let
        decodedList =
            Json.Decode.decodeValue (list fileInfoDecoder) value
    in
    case decodedList of
        Ok fileInfoList ->
            msg fileInfoList

        Err error ->
            BackendReturnedError (Json.Decode.errorToString error)


decodeFileInfo : (FileInfo -> String -> Msg) -> Json.Encode.Value -> Msg
decodeFileInfo msg value =
    let
        decodedFileInfo =
            Json.Decode.decodeValue fileInfoDecoder value

        decodedPreviousName =
            Json.Decode.decodeValue (Json.Decode.field "PreviousName" Json.Decode.string) value
    in
    case ( decodedFileInfo, decodedPreviousName ) of
        ( Ok fileInfo, Ok previousName ) ->
            msg fileInfo previousName

        ( Ok fileInfo, Err _ ) ->
            msg fileInfo ""

        ( Err error, _ ) ->
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
            case model.editedFile of
                Nothing ->
                    case model.focusedZone of
                        LeftSide ->
                            [ Events.preventDefaultOn "keydown" (keyDecoder Source)
                            ]

                        _ ->
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


keyDecoder : Target -> Json.Decode.Decoder ( Msg, Bool )
keyDecoder target =
    decodeKeyboardEvent
        |> Json.Decode.map
            (\key ->
                ( UserPressedKey target key, True )
            )


viewRightSide : Model -> Html Msg
viewRightSide model =
    let
        conditionalAttributes =
            case model.focusedZone of
                RightSide ->
                    [ Events.preventDefaultOn "keydown" (keyDecoder Destination) ]

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
    let
        currentDirName =
            String.split model.pathSeparator model.sourceDirectoryPath
                |> List.Extra.last
                -- FIXME : does it work in root dir?
                |> Maybe.withDefault "Error: cannot get current dir name"
    in
    div [ class "panel" ]
        [ div [ class "panel-header" ]
            [ h2 [] [ text <| "Source directory: " ++ currentDirName ]
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


viewDestinationSubdirectories : Model -> Html Msg
viewDestinationSubdirectories model =
    let
        currentDirName =
            String.split model.pathSeparator model.destinationDirectoryPath
                |> List.Extra.last
                |> Maybe.withDefault "Error: cannot get current dir name"
    in
    div [ class "panel" ]
        [ div [ class "panel-header" ]
            [ h2 [] [ text <| "Destination directory: " ++ currentDirName ]
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
            (model.filteredDestinationSubdirectories
                |> List.sortBy (.name >> String.toLower)
                |> List.map (viewDirectory model UserClickedDestinationDirectory)
            )
        ]


viewDirectory : Model -> (FileInfo -> Msg) -> FileInfo -> Html Msg
viewDirectory _ onClickMsg fileInfo =
    div
        [ class "fileinfo dir"
        , onClick (onClickMsg fileInfo)
        ]
        [ text <| fileInfo.name ]


viewSourceFiles : Model -> Html Msg
viewSourceFiles model =
    let
        toHighlight =
            if model.sourceReplace /= "" then
                model.sourceReplace

            else if model.sourceSearch /= "" then
                model.sourceSearch

            else
                ""
    in
    div [ class "panel" ]
        [ div [ class "panel-header" ]
            [ h2 [] [ text "Source Files" ]
            , input
                [ class "input"
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
        , div
            [ class "panel-content" ]
            (model.filteredSourceDirectoryFiles
                |> List.sortBy (.name >> String.toLower)
                |> List.map (viewFileInfo model UserClickedSourceFile toHighlight)
            )
        ]


viewDestinationFiles : Model -> Html Msg
viewDestinationFiles model =
    div [ class "panel" ]
        [ div [ class "panel-header" ]
            [ h2 [] [ text "Destination Files" ]
            ]
        , div
            [ class "panel-content" ]
            (model.destinationDirectoryFiles
                |> List.sortBy (.name >> String.toLower)
                -- TODO pass search filter as last param
                |> List.map (viewFileInfo model UserClickedDestinationFile "")
            )
        ]


viewFileInfo : Model -> (FileInfo -> Msg) -> String -> FileInfo -> Html Msg
viewFileInfo model onClickMsg highlighted fileInfo =
    let
        isEdited =
            case model.editedFile of
                Just someFileInfo ->
                    fileInfo == someFileInfo

                Nothing ->
                    False
    in
    if isEdited then
        viewEditedFilename model

    else
        viewReadOnlyFile model onClickMsg highlighted fileInfo


highlight =
    Mark.markWith { defaultOptions | minTermLength = 1 }


viewReadOnlyFile : Model -> (FileInfo -> Msg) -> String -> FileInfo -> Html Msg
viewReadOnlyFile model onClickMsg highlighted fileInfo =
    let
        className =
            if fileInfo.isSelected then
                "filename selected"

            else
                "filename"

        fileName =
            case highlighted of
                "" ->
                    [ text fileInfo.name ]

                _ ->
                    highlight highlighted fileInfo.name
    in
    div
        [ class "fileinfo"
        , onClick (onClickMsg fileInfo)
        ]
        [ div [ class className ] fileName
        , div [] [ text <| Filesize.format fileInfo.size ]
        , div [ class "filemodificationdate" ] [ viewDate model fileInfo.modTime ]
        ]


viewEditedFilename : Model -> Html Msg
viewEditedFilename model =
    form [ onSubmit UserValidatedFilename ]
        [ input
            [ autofocus True
            , class "fileinfo-input"
            , onInput UserModifiedFileName
            , onFocus (UserChangedFocusedZone NameEditor)
            , value model.editedName
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
                [ Events.preventDefaultOn "keydown" (keyDecoder Source)
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
                text errorMsg
        , case model.filesToDelete of
            [] ->
                text ""

            _ ->
                span
                    []
                    [ text "This will permanently delete the selected files. This cannot be undone."
                    , button [ class "btn", onClick UserClickedCancel ] [ text "Cancel" ]
                    , button [ class "btn", onClick UserClickedDelete, id "deleteButton" ] [ text "DELETE" ]
                    ]
        ]


viewFocusedZone : Model -> Html Msg
viewFocusedZone model =
    text <|
        "focused zone"
            ++ (case model.focusedZone of
                    Confirmation ->
                        "Confirmation | "

                    Filtering ->
                        "Filtering | "

                    LeftSide ->
                        "LeftSide | "

                    NameEditor ->
                        "NameEditor | "

                    RightSide ->
                        "RightSide | "

                    SourceSearchReplace ->
                        "SourceSearchReplace | "
               )
