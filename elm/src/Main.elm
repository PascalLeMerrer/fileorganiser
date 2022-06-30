port module Main exposing (..)

import Browser
import Html exposing (Html, button, div, input, text)
import Html.Attributes exposing (autocomplete, class, id, type_, value)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (required)
import Json.Encode


type alias FileInfo =
    { name : String
    , size : Int
    , mode : Int
    , modTime : String
    , isDir : Bool
    }


fileInfoDecoder : Decoder FileInfo
fileInfoDecoder =
    Json.Decode.succeed FileInfo
        |> required "Name" Json.Decode.string
        |> required "Size" Json.Decode.int
        |> required "Mode" Json.Decode.int
        |> required "ModTime" Json.Decode.string
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
    , sourceDirectoryPath : String
    , sourceDirectoryContent : List FileInfo
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { error = Nothing
      , sourceDirectoryPath = "."
      , sourceDirectoryContent = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = BackendReturnedError String
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
        [ div [ class "input-box", id "input" ]
            [ input
                [ class "input"
                , id "name"
                , type_ "text"
                , autocomplete False
                , onInput SetSourceDirectory
                , value model.sourceDirectoryPath
                ]
                []
            , button [ class "btn", onClick Submit ] [ text "Open" ]
            ]
        , case model.error of
            Nothing ->
                viewFiles model

            Just errorMsg ->
                text errorMsg
        ]


viewFiles : Model -> Html Msg
viewFiles model =
    div [] <|
        List.map viewFileInfo model.sourceDirectoryContent


viewFileInfo : FileInfo -> Html Msg
viewFileInfo fileInfo =
    div [] [ text <| fileInfo.name ++ " " ++ fileInfo.modTime ++ " " ++ String.fromInt fileInfo.size ]
