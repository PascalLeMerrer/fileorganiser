port module Main exposing (..)

import Browser
import Html exposing (Html, aside, button, div, footer, form, h2, header, input, main_, text)
import Html.Attributes exposing (autocomplete, class, id, type_, value)
import Html.Events exposing (onInput, onSubmit)
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


port loadDirectoryContent : String -> Cmd msg


port getCurrentDirectoryPath : () -> Cmd msg


port receiveCurrentDirectoryPath : (String -> msg) -> Sub msg


port receiveDirectoryContent : (Json.Encode.Value -> msg) -> Sub msg


port receiveError : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { error : Maybe String
    , pathSeparator : String
    , sourceDirectoryFiles : List FileInfo
    , sourceDirectoryPath : String
    , sourceSubDirectories : List FileInfo
    , timezone : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { error = Nothing
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
    | BackendReturnedSourceDirectoryContent (List FileInfo)
    | SetSourceDirectory String
    | Submit
    | NoOp


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
            , loadDirectoryContent path
            )

        BackendReturnedError errorMsg ->
            ( { model | error = Just errorMsg }, Cmd.none )

        BackendReturnedSourceDirectoryContent directoryContent ->
            let
                ( dirList, fileList ) =
                    List.partition .isDir directoryContent
            in
            ( { model | sourceSubDirectories = dirList, sourceDirectoryFiles = fileList }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )

        SetSourceDirectory path ->
            ( { model | sourceDirectoryPath = path }, Cmd.none )

        Submit ->
            ( model, loadDirectoryContent model.sourceDirectoryPath )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveDirectoryContent decodeFileInfoList
        , receiveError BackendReturnedError
        , receiveCurrentDirectoryPath BackendReturnedCurrentDirPath
        ]


decodeFileInfoList : Json.Encode.Value -> Msg
decodeFileInfoList value =
    let
        decodedList =
            Json.Decode.decodeValue (list fileInfoDecoder) value
    in
    case decodedList of
        Ok fileInfoList ->
            BackendReturnedSourceDirectoryContent fileInfoList

        Err error ->
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
        [ form
            [ class "input-box"
            , onSubmit Submit
            ]
            [ input
                [ class "input"
                , id "name"
                , type_ "text"
                , autocomplete False
                , onInput SetSourceDirectory
                , value model.sourceDirectoryPath
                ]
                []
            , button [ class "btn", type_ "submit" ] [ text "Open" ]
            ]
        ]


viewLeftSide : Model -> Html Msg
viewLeftSide model =
    aside [] <| viewFiles model


viewRightSide : Model -> Html Msg
viewRightSide model =
    main_ [] []


viewFiles : Model -> List (Html Msg)
viewFiles model =
    [ viewSourceSubirectories model
    , viewSourceFiles model
    ]


viewSourceSubirectories : Model -> Html Msg
viewSourceSubirectories model =
    let
        currentDirName =
            String.split model.pathSeparator model.sourceDirectoryPath
                |> List.Extra.last
                |> Maybe.withDefault "Error: cannot get current dir name"
    in
    div [ class "panel" ] <|
        [ h2 [] [ text <| "Source directory: " ++ currentDirName ]
        ]
            ++ (model.sourceSubDirectories
                    |> List.sortBy (.name >> String.toLower)
                    |> List.map (viewFileInfo model)
               )


viewSourceFiles : Model -> Html Msg
viewSourceFiles model =
    div [ class "panel" ] <|
        [ h2 [] [ text "Source Files" ]
        ]
            ++ (model.sourceDirectoryFiles
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
