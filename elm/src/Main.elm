port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, footer, h2, header, input, text)
import Html.Attributes exposing (autocomplete, class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso8601
import Json.Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode
import Keyboard exposing (Key(..), KeyChange(..))
import List.Extra
import Task
import Time exposing (Month(..), Posix)



-- antislash is doubled for escaping


windowsPathSep =
    "\\"


unixPathSep =
    "/"


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



-- PORTS


port getCurrentDirectoryPath : () -> Cmd msg


port getDestinationDirectoryFiles : String -> Cmd msg


port getSourceDirectoryContent : String -> Cmd msg


port getSubdirectories : String -> Cmd msg


port moveFiles : ( List String, String ) -> Cmd msg


port receiveCurrentDirectoryPath : (String -> msg) -> Sub msg


port receiveDestinationDirectoryFiles : (Json.Encode.Value -> msg) -> Sub msg


port receiveError : (String -> msg) -> Sub msg


port receiveMovedFiles : (Json.Encode.Value -> msg) -> Sub msg


port receiveSelectedDestinationDirectory : (String -> msg) -> Sub msg


port receiveSelectedSourceDirectory : (String -> msg) -> Sub msg


port receiveSourceDirectoryContent : (Json.Encode.Value -> msg) -> Sub msg


port receiveSubDirectories : (Json.Encode.Value -> msg) -> Sub msg


port selectDestinationDirectory : String -> Cmd msg


port selectSourceDirectory : String -> Cmd msg



-- MODEL


type alias Model =
    { destinationDirectoryFiles : List FileInfo
    , destinationDirectoryPath : String
    , destinationSubdirectories : List FileInfo
    , error : Maybe String
    , filter : String
    , filteredDestinationSubdirectories : List FileInfo
    , filteredSourceDirectoryFiles : List FileInfo
    , history : List Command
    , pathSeparator : String
    , pressedKeys : List Key
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
      , error = Nothing
      , filter = ""
      , filteredDestinationSubdirectories = []
      , filteredSourceDirectoryFiles = []
      , history = []
      , pathSeparator = unixPathSep
      , pressedKeys = []
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
    | BackendReturnedDestinationFiles (List FileInfo)
    | BackendReturnedDestinationDirectoryPath String
    | BackendReturnedError String
    | BackendReturnedMovedFiles (List FileInfo)
    | BackendReturnedSourceDirectoryContent (List FileInfo)
    | BackendReturnedSourceDirectoryPath String
    | UserPressedKey Keyboard.Msg
    | NoOp
    | UserChangedFilter String
    | UserClickedClearFilter
    | UserClickedDestinationDirectoryButton
    | UserClickedSourceFile FileInfo
    | UserClickedDestinationFile FileInfo
    | UserClickedSourceDirectoryButton


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
                , pathSeparator = pathSeparator
              }
            , Cmd.batch
                [ getSourceDirectoryContent path
                , getSubdirectories path
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
                    [ getSubdirectories path
                    , getDestinationDirectoryFiles path
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

        UserPressedKey keyMsg ->
            let
                ( pressedKeys, maybeKeyChange ) =
                    Keyboard.updateWithKeyChange Keyboard.anyKeyOriginal keyMsg model.pressedKeys

                modelWithPressedKeys =
                    { model
                        | pressedKeys = Debug.log "pressedKeys" pressedKeys
                    }
            in
            case Debug.log "maybeKeyChange" maybeKeyChange of
                Just (KeyDown (Character "m")) ->
                    moveSelectedSourceFiles modelWithPressedKeys

                Just (KeyDown (Character "u")) ->
                    cancelMove modelWithPressedKeys

                _ ->
                    ( model, Cmd.none )

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


cancelMove : Model -> ( Model, Cmd Msg )
cancelMove model =
    let
        commandToCancel =
            model.history
                |> List.head
    in
    case commandToCancel of
        Just command ->
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

        Nothing ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveCurrentDirectoryPath BackendReturnedCurrentDirPath
        , receiveSourceDirectoryContent (decodeFileInfoList BackendReturnedSourceDirectoryContent)
        , receiveDestinationDirectoryFiles (decodeFileInfoList BackendReturnedDestinationFiles)
        , receiveError BackendReturnedError
        , receiveMovedFiles (decodeFileInfoList BackendReturnedMovedFiles)
        , receiveSelectedDestinationDirectory BackendReturnedDestinationDirectoryPath
        , receiveSelectedSourceDirectory BackendReturnedSourceDirectoryPath
        , receiveSubDirectories (decodeFileInfoList BackendReturnedDestinationDirectories)
        , Sub.map UserPressedKey Keyboard.subscriptions
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
            -- TODO process error
            NoOp



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
    header []
        [ div
            [ class "input-box"
            ]
            [ input
                [ class "input"
                , id "filter"
                , type_ "text"
                , autocomplete False
                , onInput UserChangedFilter
                , value model.filter
                , placeholder "Enter one or more words to filter source files and destination directories"
                ]
                []
            , button [ class "btn", onClick UserClickedClearFilter ] [ text "Clear" ]
            ]
        ]


viewLeftSide : Model -> Html Msg
viewLeftSide model =
    div [ id "container-left" ] <|
        viewSource model


viewRightSide : Model -> Html Msg
viewRightSide model =
    div [ id "container-right" ] <|
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
                |> List.map (viewFileInfo model UserClickedSourceFile)
             -- FIXME
            )
        ]


viewDestinationSubdirectories : Model -> Html Msg
viewDestinationSubdirectories model =
    div [ class "panel" ]
        [ div [ class "panel-header" ]
            [ h2 [] [ text <| "Destination subdirectories " ]
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
                |> List.map (viewFileInfo model UserClickedSourceFile)
             -- FIXME
            )
        ]


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
    if fileInfo.isDir then
        div
            [ class "fileinfo"
            , onClick (onClickMsg fileInfo)
            ]
            [ text <| fileInfo.name ]

    else
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
            , div [] [ text <| String.fromInt fileInfo.size ]
            , div [ class "filemodificationdate" ] [ viewDate model fileInfo.modTime ]
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
    footer []
        [ case model.error of
            Nothing ->
                text "TODO display help"

            Just errorMsg ->
                text errorMsg
        ]
