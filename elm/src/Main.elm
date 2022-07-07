port module Main exposing (..)

import Browser
import Filesize
import Html exposing (Html, button, div, footer, form, h2, header, input, span, text)
import Html.Attributes exposing (autocomplete, autofocus, class, id, placeholder, tabindex, type_, value)
import Html.Events as Events exposing (onClick, onFocus, onInput, onSubmit)
import Iso8601
import Json.Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import List.Extra
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
    { destinationDirectoryFiles : List FileInfo
    , destinationDirectoryPath : String
    , destinationSubdirectories : List FileInfo
    , editedFile : Maybe FileInfo
    , editedName : String
    , error : Maybe String
    , filesToDelete : List FileInfo
    , filter : String
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
    ( { destinationDirectoryFiles = []
      , destinationDirectoryPath = ""
      , destinationSubdirectories = []
      , editedFile = Nothing
      , editedName = ""
      , error = Nothing
      , filesToDelete = []
      , filter = ""
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
    | BackendReturnedRemovedFile FileInfo
    | BackendReturnedRenamedFile FileInfo
    | BackendReturnedSourceDirectoryContent (List FileInfo)
    | BackendReturnedSourceDirectoryPath String
    | NoOp
    | UserChangedFilter String
    | UserClickedCancel
    | UserClickedClearFilter
    | UserClickedDelete
    | UserClickedDestinationDirectory FileInfo
    | UserClickedDestinationDirectoryButton
    | UserClickedDestinationFile FileInfo
    | UserClickedSourceDirectory FileInfo
    | UserClickedSourceDirectoryButton
    | UserClickedSourceFile FileInfo
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

        BackendReturnedRemovedFile fileInfo ->
            ( model, getSourceDirectoryContent model.sourceDirectoryPath )

        BackendReturnedRenamedFile fileInfo ->
            case model.editedFile of
                Just editedFile ->
                    let
                        newFileInfo =
                            { fileInfo | isSelected = True }

                        originalPath =
                            model.sourceDirectoryPath
                                ++ model.pathSeparator
                                ++ editedFile.name

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
                    ( { model | error = Just ("File was unexpectedLy renamed to " ++ fileInfo.name) }
                    , Cmd.none
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

        UserChangedFilter filteringString ->
            ( { model | filter = filteringString }
                |> filterSourceFiles
                |> filterDestinationDirectories
            , Cmd.none
            )

        UserClickedClearFilter ->
            ( { model | filter = "" } |> filterSourceFiles
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
            applyRenaming model

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
            String.words model.filter
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
            String.words model.filter
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
    List.all (\word -> String.contains word fileInfo.name) filters


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


applyRenaming : Model -> ( Model, Cmd Msg )
applyRenaming model =
    let
        oldName =
            model.editedFile
                |> Maybe.map (\f -> model.sourceDirectoryPath ++ model.pathSeparator ++ f.name)
                |> Maybe.withDefault ""

        newName =
            model.sourceDirectoryPath ++ model.pathSeparator ++ model.editedName

        encodedValue =
            Json.Encode.object
                [ ( "oldName", Json.Encode.string oldName )
                , ( "newName", Json.Encode.string newName )
                ]
    in
    ( model, renameFile encodedValue )


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
                -- TODO should we selected filtered files only?
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

        ( Key.M, False, False ) ->
            moveSelectedSourceFiles model

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


decodeFileInfo : (FileInfo -> Msg) -> Json.Encode.Value -> Msg
decodeFileInfo msg value =
    let
        decodedValue =
            Json.Decode.decodeValue fileInfoDecoder value
    in
    case decodedValue of
        Ok fileInfo ->
            msg fileInfo

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
                , id "filter"
                , type_ "text"
                , autocomplete False
                , onInput UserChangedFilter
                , onFocus (UserChangedFocusedZone Filtering)

                --, onBlur (UserChangedFocusedZone LeftSide)
                , value model.filter
                , placeholder "Enter one or more words to filter source files and destination directories"
                ]
                []
            , button [ class "btn", onClick UserClickedClearFilter ] [ text "Clear" ]
            ]
        ]


viewLeftSide : Model -> Html Msg
viewLeftSide model =
    let
        conditionalAttributes =
            case model.editedFile of
                Just a ->
                    [ onFocus (UserChangedFocusedZone NameEditor) ]

                Nothing ->
                    [ Events.preventDefaultOn "keydown" (keyDecoder Source)
                    , onFocus (UserChangedFocusedZone LeftSide)
                    ]
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
            case model.editedFile of
                Just a ->
                    []

                Nothing ->
                    [ Events.preventDefaultOn "keydown" (keyDecoder Destination) ]
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
                |> Maybe.withDefault "Error: cannot get current dir name"
    in
    div [ class "panel" ]
        [ div [ class "panel-header" ]
            [ h2 [] [ text <| "Source directory: " ++ currentDirName ]
            , button
                [ class "btn"
                , onClick UserClickedSourceDirectoryButton
                ]
                [ text "..." ]
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
            , button
                [ class "btn"
                , onClick UserClickedDestinationDirectoryButton
                ]
                [ text "..." ]
            ]
        , div
            [ class "panel-content" ]
            (model.filteredDestinationSubdirectories
                |> List.sortBy (.name >> String.toLower)
                |> List.map (viewDirectory model UserClickedDestinationDirectory)
            )
        ]


viewDirectory : Model -> (FileInfo -> Msg) -> FileInfo -> Html Msg
viewDirectory model onClickMsg fileInfo =
    div
        [ class "fileinfo"
        , onClick (onClickMsg fileInfo)
        ]
        [ text <| fileInfo.name ]


viewSourceFiles : Model -> Html Msg
viewSourceFiles model =
    div [ class "panel" ]
        [ div [ class "panel-header" ]
            [ h2 [] [ text "Source Files" ]
            ]
        , div
            [ class "panel-content" ]
            (model.filteredSourceDirectoryFiles
                |> List.sortBy (.name >> String.toLower)
                |> List.map (viewFileInfo model UserClickedSourceFile)
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
                |> List.map (viewFileInfo model UserClickedDestinationFile)
             --FIXME UserClickedDestinationFile
            )
        ]


viewFileInfo : Model -> (FileInfo -> Msg) -> FileInfo -> Html Msg
viewFileInfo model onClickMsg fileInfo =
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
        viewReadOnlyFile model onClickMsg fileInfo


viewReadOnlyFile : Model -> (FileInfo -> Msg) -> FileInfo -> Html Msg
viewReadOnlyFile model onClickMsg fileInfo =
    let
        className =
            if fileInfo.isSelected then
                "filename selected"

            else
                "filename"
    in
    div
        [ class "fileinfo"
        , onClick (onClickMsg fileInfo)
        ]
        [ div [ class className ] [ text fileInfo.name ]
        , div [] [ text <| Filesize.format fileInfo.size ]
        , div [ class "filemodificationdate" ] [ viewDate model fileInfo.modTime ]
        ]


viewEditedFilename : Model -> Html Msg
viewEditedFilename model =
    form [ onSubmit UserValidatedFilename ]
        [ input
            [ class "fileinfo-input"
            , onInput UserModifiedFileName
            , autofocus True
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
               )
