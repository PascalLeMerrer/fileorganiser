port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, footer, h2, header, input, text)
import Html.Attributes exposing (autocomplete, class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso8601
import Json.Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode
import List.Extra
import Task
import Time exposing (Month(..), Posix)


windowsPathSep =
    "\\"



-- antislash is doubled for escaping it


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


port getDirectoryContent : String -> Cmd msg


port getSubdirectories : String -> Cmd msg


port getCurrentDirectoryPath : () -> Cmd msg


port receiveCurrentDirectoryPath : (String -> msg) -> Sub msg


port receiveDirectoryContent : (Json.Encode.Value -> msg) -> Sub msg


port receiveError : (String -> msg) -> Sub msg


port receiveSelectedDestinationDirectory : (String -> msg) -> Sub msg


port receiveSelectedSourceDirectory : (String -> msg) -> Sub msg


port receiveSubDirectories : (Json.Encode.Value -> msg) -> Sub msg


port selectSourceDirectory : String -> Cmd msg


port selectDestinationDirectory : String -> Cmd msg



-- MODEL


type alias Model =
    { destinationDirectoryFiles : List FileInfo
    , destinationDirectoryPath : String
    , destinationSubdirectories : List FileInfo
    , error : Maybe String
    , filter : String
    , filteredDestinationSubdirectories : List FileInfo
    , filteredSourceDirectoryFiles : List FileInfo
    , pathSeparator : String
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
      , pathSeparator = unixPathSep
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
    | BackendReturnedError String
    | BackendReturnedSourceDirectoryContent (List FileInfo)
    | BackendReturnedSourceDirectoryPath String
    | NoOp
    | UserChangedFilter String
    | UserClickedClearFilter
    | UserClickedDestinationDirectoryButton
    | UserClickedSourceFile FileInfo
    | UserClickedDestinationFile FileInfo
    | UserClickedSourceDirectoryButton


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
                , pathSeparator = pathSeparator
              }
            , Cmd.batch
                [ getDirectoryContent path
                , getSubdirectories path
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

        BackendReturnedDestinationDirectoryPath path ->
            if path == "" then
                -- user canceled the selection
                ( model, Cmd.none )

            else
                ( { model | destinationDirectoryPath = path }
                , getSubdirectories path
                )

        BackendReturnedSourceDirectoryPath path ->
            if path == "" then
                -- user canceled the selection
                ( model, Cmd.none )

            else
                ( { model | sourceDirectoryPath = path }
                , getDirectoryContent path
                )

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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveCurrentDirectoryPath BackendReturnedCurrentDirPath
        , receiveDirectoryContent (decodeFileInfoList BackendReturnedSourceDirectoryContent)
        , receiveError BackendReturnedError
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
