port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, footer, h2, header, input, text)
import Html.Attributes exposing (autocomplete, class, id, placeholder, type_, value)
import Html.Events exposing (onClick, onInput)
import Iso8601
import Json.Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (required)
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
    { name : String
    , size : Int
    , mode : Int
    , modTime : Posix
    , isDir : Bool
    }


fileInfoDecoder : Decoder FileInfo
fileInfoDecoder =
    Json.Decode.succeed FileInfo
        |> required "Name" Json.Decode.string
        |> required "Size" Json.Decode.int
        |> required "Mode" Json.Decode.int
        |> required "ModTime" Iso8601.decoder
        |> required "IsDir" Json.Decode.bool



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


port receiveSelectedSourceDirectory : (String -> msg) -> Sub msg


port receiveSubDirectories : (Json.Encode.Value -> msg) -> Sub msg


port selectSourceDirectory : String -> Cmd msg



-- MODEL


type alias Model =
    { destinationDirectoryFiles : List FileInfo
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
    | BackendReturnedError String
    | BackendReturnedCurrentDirPath String
    | BackendReturnedSourceDirectoryPath String
    | BackendReturnedSourceDirectoryContent (List FileInfo)
    | BackendReturnedDestinationDirectories (List FileInfo)
    | UserChangedFilter String
    | UserClickedClearFilter
    | UserClickedSourceDirectoryButton
    | NoOp


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
        [ receiveDirectoryContent (decodeFileInfoList BackendReturnedSourceDirectoryContent)
        , receiveError BackendReturnedError
        , receiveCurrentDirectoryPath BackendReturnedCurrentDirPath
        , receiveSubDirectories (decodeFileInfoList BackendReturnedDestinationDirectories)
        , receiveSelectedSourceDirectory BackendReturnedSourceDirectoryPath
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
    div [ class "panel" ] <|
        [ h2 [] [ text <| "Source directory: " ++ currentDirName ]
        , button [ onClick UserClickedSourceDirectoryButton ] [ text "..." ]
        ]
            ++ (model.sourceSubDirectories
                    |> List.sortBy (.name >> String.toLower)
                    |> List.map (viewFileInfo model)
               )


viewDestinationSubdirectories : Model -> Html Msg
viewDestinationSubdirectories model =
    div [ class "panel" ] <|
        [ h2 [] [ text <| "Destination subdirectories " ]
        ]
            ++ (model.filteredDestinationSubdirectories
                    |> List.sortBy (.name >> String.toLower)
                    |> List.map (viewFileInfo model)
               )


viewSourceFiles : Model -> Html Msg
viewSourceFiles model =
    div [ class "panel" ] <|
        [ h2 [] [ text "Source Files" ]
        ]
            ++ (model.filteredSourceDirectoryFiles
                    |> List.sortBy (.name >> String.toLower)
                    |> List.map (viewFileInfo model)
               )


viewDestinationFiles : Model -> Html Msg
viewDestinationFiles model =
    div [ class "panel" ] <|
        [ h2 [] [ text "Destination Files" ]
        ]
            ++ (model.destinationDirectoryFiles
                    |> List.sortBy (.name >> String.toLower)
                    |> List.map (viewFileInfo model)
               )


viewFileInfo : Model -> FileInfo -> Html Msg
viewFileInfo model fileInfo =
    if fileInfo.isDir then
        div [ class "fileinfo" ] [ text <| fileInfo.name ]

    else
        div [ class "fileinfo" ]
            [ div [ class "filename" ] [ text fileInfo.name ]
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
