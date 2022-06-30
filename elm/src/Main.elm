port module Main exposing (..)

import Browser
import Html exposing (Html, aside, button, div, footer, form, h2, header, input, main_, text)
import Html.Attributes exposing (autocomplete, class, id, type_, value)
import Html.Events exposing (onInput, onSubmit)
import Iso8601
import Json.Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (required)
import Json.Encode
import Task
import Time exposing (Month(..), Posix)


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


port receiveDirectoryContent : (Json.Encode.Value -> msg) -> Sub msg


port receiveError : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { error : Maybe String
    , sourceDirectoryContent : List FileInfo
    , sourceDirectoryPath : String
    , timezone : Time.Zone
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { error = Nothing
      , sourceDirectoryContent = []
      , sourceDirectoryPath = "."
      , timezone = Time.utc
      }
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = AdjustTimeZone Time.Zone
    | BackendReturnedError String
    | SetSourceDirectory String
    | Submit
    | FileInfoReceived (List FileInfo)
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSourceDirectory path ->
            ( { model | sourceDirectoryPath = path }, Cmd.none )

        Submit ->
            ( model, loadDirectoryContent model.sourceDirectoryPath )

        FileInfoReceived directoryContent ->
            ( { model | sourceDirectoryContent = directoryContent }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )

        BackendReturnedError errorMsg ->
            ( { model | error = Just errorMsg }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | timezone = newZone }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ receiveDirectoryContent decodeFileInfoList
        , receiveError BackendReturnedError
        ]


decodeFileInfoList : Json.Encode.Value -> Msg
decodeFileInfoList value =
    let
        decodedList =
            Json.Decode.decodeValue (list fileInfoDecoder) value
    in
    case Debug.log "decodedList" decodedList of
        Ok fileInfoList ->
            FileInfoReceived fileInfoList

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
    aside []
        [ case model.error of
            Nothing ->
                viewFiles model

            Just errorMsg ->
                text errorMsg
        ]


viewRightSide : Model -> Html Msg
viewRightSide model =
    main_ [] []


viewFiles : Model -> Html Msg
viewFiles model =
    div [] <|
        [ h2 [] [ text "Source Directory" ]
        ]
            ++ List.map (viewFileInfo model) model.sourceDirectoryContent


viewFileInfo : Model -> FileInfo -> Html Msg
viewFileInfo model fileInfo =
    if fileInfo.isDir then
        div [] [ text <| fileInfo.name ]

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
