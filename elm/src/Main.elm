port module Main exposing (Command, File, FileStatus(..), FocusedZone, Model, Msg, Operation, defaultDir, defaultModel, filterDestinationDirectories, main, pathElements, select, truncateConcatenatedNames, withName, withParentPath, withStatus)

import Browser
import Browser.Dom
import Filesize
import Html exposing (Html, button, div, footer, form, h2, input, span, text)
import Html.Attributes exposing (class, disabled, id, placeholder, tabindex, type_, value)
import Html.Events as Events exposing (onClick, onFocus, onInput, onSubmit)
import Iso8601
import Json.Decode exposing (Decoder, list)
import Json.Decode.Pipeline exposing (hardcoded, required)
import Json.Encode
import Keyboard.Event exposing (KeyboardEvent, decodeKeyboardEvent)
import Keyboard.Key as Key
import List.Extra
import Pattern
import Regex
import String.Mark as Mark exposing (defaultOptions, multiWord)
import Task
import Time exposing (Month(..), Posix, millisToPosix)



-- antislash is doubled for escaping


port createDirectory : String -> Cmd msg


port fileRemoved : (Json.Encode.Value -> msg) -> Sub msg



{- Maps Golang File -}


port filesRenamed : (Json.Encode.Value -> msg) -> Sub msg


port getCurrentDirectoryPath : () -> Cmd msg


port getDestinationDirectoryFiles : String -> Cmd msg


port getDestinationSubdirectories : String -> Cmd msg


port getSourceDirectoryContent : String -> Cmd msg


port moveFiles : ( List String, String ) -> Cmd msg



-- MAIN


port openFile : String -> Cmd msg



-- OUTPUT PORTS


port quit : () -> Cmd msg


port receiveCreatedDirectory : (Json.Encode.Value -> msg) -> Sub msg


port receiveCurrentDirectoryPath : (String -> msg) -> Sub msg


port receiveDestinationDirectoryFiles : (Json.Encode.Value -> msg) -> Sub msg


port receiveError : (String -> msg) -> Sub msg


port receiveMovedFiles : (Json.Encode.Value -> msg) -> Sub msg


port receiveSelectedDestinationDirectory : (String -> msg) -> Sub msg


port receiveSelectedSourceDirectory : (String -> msg) -> Sub msg


port receiveSourceDirectoryContent : (Json.Encode.Value -> msg) -> Sub msg


port receiveSubDirectories : (Json.Encode.Value -> msg) -> Sub msg


port removeFile : Json.Encode.Value -> Cmd msg


port renameFiles : List Json.Encode.Value -> Cmd msg



-- INPUT PORTS


port selectDestinationDirectory : String -> Cmd msg


port selectSourceDirectory : String -> Cmd msg


type alias Command =
    { operation : Operation
    , files : List File -- the files / dirs affected by the operation
    , destination : Maybe String -- destination dir if any FIXME this is a bad design; try to create specific commands instead
    , source : Maybe String -- source dir if any FIXME this is a bad design; try to create specific commands instead
    }



-- TODO move to its own module


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


type FocusedZone
    = Confirmation
    | ErrorMessage
    | Filtering
    | LeftSide
    | FileNameEditor
    | DirNameEditor
    | RightSide
    | SourceSearchReplace


type alias Model =
    { applySourceFilterToDestinationDirectories : Bool
    , applySourceFilterToDestinationFiles : Bool
    , destinationDirectoryPath : String
    , destinationDirectoryFilter : String
    , destinationFiles : List File
    , destinationFilesFilter : String
    , destinationNavigationHistory : List String
    , destinationSubdirectories : List File
    , editedFile : Maybe File
    , editedDirName : String
    , editedFileName : String
    , error : Maybe String
    , filesToDelete : List File
    , focusedZone : FocusedZone
    , history : List (List Command)
    , isCreatingDirectory : Bool
    , isShiftPressed : Bool
    , isControlPressed : Bool
    , isUndoing : Bool
    , previousFocusedZone : FocusedZone
    , pathSeparator : String
    , sourceDirectoryPath : String
    , sourceFiles : List File
    , sourceFilter : String
    , sourceNavigationHistory : List String
    , sourceReplace : String
    , sourceSearch : String
    , sourceSubDirectories : List File
    , timezone : Time.Zone
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
    | BackendReturnedRemovedFile
    | BackendReturnedRenamedFiles (List File) (List String)
    | BackendReturnedSourceDirectoryContent (List File)
    | BackendReturnedSourceDirectoryPath String
    | NoOp
    | UserChangedDestinationDirectoryFilter String
    | UserChangedDestinationFilesFilter String
    | UserChangedSourceFilter String
    | UserChangedSourceReplace String
    | UserChangedSourceSearch String
    | UserClickedCancel
    | UserClickedClearSourceFilter
    | UserClickedClearDestinationDirectoryFilter
    | UserClickedClearDestinationFilesFilter
    | UserClickedCloseError
    | UserClickedDelete
    | UserClickedDestinationDirectory Bool File -- isAbsolute,  destination
    | UserClickedDestinationDirectoryButton
    | UserClickedDestinationFile File
    | UserClickedReload Target
    | UserClickedReplaceButton
    | UserClickedSourceDirectory Bool File -- isAbsolute, source
    | UserClickedSourceDirectoryButton
    | UserClickedSourceFile File
    | UserClickedSynchronizeDirFilterButton
    | UserClickedSynchronizeFileFilterButton
    | UserChangedFocusedZone FocusedZone
    | UserModifiedFileName String
    | UserModifiedDirName String
    | UserPressedKey Target KeyboardEvent
    | UserPressedOrReleasedKey KeyboardEvent
    | UserSubmittedDirName
    | UserSubmittedFilename


type Operation
    = Move
    | Rename


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


defaultModel : Model
defaultModel =
    { applySourceFilterToDestinationDirectories = False
    , applySourceFilterToDestinationFiles = False
    , destinationDirectoryPath = ""
    , destinationDirectoryFilter = ""
    , destinationFiles = []
    , destinationFilesFilter = ""
    , destinationNavigationHistory = []
    , destinationSubdirectories = []
    , editedFile = Nothing
    , editedDirName = ""
    , editedFileName = ""
    , error = Nothing
    , filesToDelete = []
    , focusedZone = LeftSide
    , history = []
    , isCreatingDirectory = False
    , isShiftPressed = False
    , isControlPressed = False
    , isUndoing = False
    , previousFocusedZone = LeftSide
    , pathSeparator = unixPathSep
    , sourceDirectoryPath = "."
    , sourceFiles = []
    , sourceFilter = ""
    , sourceNavigationHistory = []
    , sourceReplace = ""
    , sourceSearch = ""
    , sourceSubDirectories = []
    , timezone = Time.utc
    }


filterDestinationDirectories : Model -> Model
filterDestinationDirectories model =
    let
        words : List String
        words =
            String.words model.destinationDirectoryFilter
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


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }


pathElements : Model -> List File -> String -> List File
pathElements model acc path =
    if path == "." then
        []

    else
        pathElementsRecursive model acc path
            |> List.reverse



-- MODEL


select : Model -> List File -> File -> List File
select model files clikedFile =
    if model.isShiftPressed then
        let
            indexOfClickedElement : Int
            indexOfClickedElement =
                List.Extra.elemIndex clikedFile files
                    |> Maybe.withDefault 0

            indexOfFirstSelectedElement : Int
            indexOfFirstSelectedElement =
                selectedItemsIndices
                    |> List.minimum
                    |> Maybe.withDefault 0

            indexOfLastSelectedElement : Int
            indexOfLastSelectedElement =
                selectedItemsIndices
                    |> List.maximum
                    |> Maybe.withDefault 0

            selectedItemsIndices : List Int
            selectedItemsIndices =
                List.Extra.findIndices (\f -> f.status == Selected) files
        in
        files
            |> List.Extra.updateIfIndex
                (\index ->
                    let
                        isInRangeBetweenClickedFileAndLastSelectedFile : Bool
                        isInRangeBetweenClickedFileAndLastSelectedFile =
                            indexOfClickedElement
                                < indexOfLastSelectedElement
                                && index
                                >= indexOfClickedElement
                                && index
                                <= indexOfLastSelectedElement

                        isInRangeBetweenFirstSelectedFileAndClickedFile : Bool
                        isInRangeBetweenFirstSelectedFileAndClickedFile =
                            indexOfClickedElement > indexOfFirstSelectedElement && index >= indexOfFirstSelectedElement && index <= indexOfClickedElement
                    in
                    isInRangeBetweenFirstSelectedFileAndClickedFile
                        || (not isInRangeBetweenFirstSelectedFileAndClickedFile && isInRangeBetweenClickedFileAndLastSelectedFile)
                )
                (\f ->
                    if f.satisfiesFilter then
                        { f | status = Selected }

                    else
                        f
                )

    else
        files
            |> List.map
                (\f ->
                    if f == clikedFile then
                        toggleSelectionStatus f

                    else if model.isControlPressed then
                        f

                    else
                        f |> withStatus Unselected
                )


truncateConcatenatedNames : Int -> List File -> List File
truncateConcatenatedNames maxLength elements =
    let
        currentLength : List File -> Int
        currentLength acc =
            acc
                |> List.map (.name >> String.length)
                |> List.sum
    in
    elements
        |> List.reverse
        |> List.foldl
            (\element acc ->
                if currentLength acc + String.length element.name <= maxLength then
                    element :: acc

                else
                    acc
            )
            []


withName : String -> File -> File
withName name file =
    { file | name = name }



-- UPDATE


withParentPath : String -> File -> File
withParentPath path file =
    { file | parentPath = path }


withStatus : FileStatus -> File -> File
withStatus fileStatus file =
    { file | status = fileStatus }


additionalHeaderClass : Model -> FocusedZone -> String
additionalHeaderClass model zone =
    if model.focusedZone == zone then
        " focused"

    else
        " unfocused"


applyRenaming : Model -> List Renaming -> Cmd Msg
applyRenaming model renamings =
    let
        encodedRenamings : List Json.Encode.Value
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


changeAllFileStatus : Model -> Target -> FileStatus -> ( Model, Cmd Msg )
changeAllFileStatus model target status =
    case target of
        Source ->
            let
                updatedFiles : List File
                updatedFiles =
                    model.sourceFiles
                        |> List.map
                            (\f ->
                                if f.satisfiesFilter then
                                    { f | status = status }

                                else
                                    f
                            )
            in
            ( { model
                | sourceFiles = updatedFiles
              }
                |> filterSourceFiles
            , Cmd.none
            )

        Destination ->
            let
                updatedFiles : List File
                updatedFiles =
                    model.destinationFiles
                        |> List.map
                            (\f -> { f | status = status })
            in
            ( { model
                | destinationFiles = updatedFiles
              }
            , Cmd.none
            )


changeDestinationDirectory : String -> Model -> Model
changeDestinationDirectory path model =
    { model
        | destinationDirectoryPath = path
        , destinationNavigationHistory = model.destinationDirectoryPath :: model.destinationNavigationHistory
    }


changeSourceDirectory : String -> Model -> Model
changeSourceDirectory path model =
    { model
        | sourceDirectoryPath = path
        , sourceNavigationHistory = model.sourceDirectoryPath :: model.sourceNavigationHistory
    }


changeStatusOfDestinationFiles : FileStatus -> FileStatus -> Model -> Model
changeStatusOfDestinationFiles fromStatus toStatus model =
    { model
        | destinationFiles =
            List.map
                (\file ->
                    if file.status == fromStatus then
                        { file | status = toStatus }

                    else
                        file
                )
                model.destinationFiles
    }


changeStatusOfSourceFiles : FileStatus -> FileStatus -> Model -> Model
changeStatusOfSourceFiles fromStatus toStatus model =
    { model
        | sourceFiles =
            List.map
                (\file ->
                    if file.satisfiesFilter && file.status == fromStatus then
                        { file | status = toStatus }

                    else
                        file
                )
                model.sourceFiles
    }
        |> filterSourceFiles


createNewDirectory : Model -> ( Model, Cmd Msg )
createNewDirectory model =
    let
        dirPath : String
        dirPath =
            model.destinationDirectoryPath
                ++ model.pathSeparator
                ++ model.editedDirName
    in
    ( model, createDirectory dirPath )


decodeFile : (File -> Msg) -> Json.Encode.Value -> Msg
decodeFile msg value =
    let
        decodedFile : Result Json.Decode.Error File
        decodedFile =
            Json.Decode.decodeValue fileDecoder value
    in
    case decodedFile of
        Ok file ->
            msg file

        Err error ->
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


decodeFileWithPreviousName : Msg -> Json.Encode.Value -> Msg
decodeFileWithPreviousName msg value =
    let
        decodedFile : Result Json.Decode.Error File
        decodedFile =
            Json.Decode.decodeValue fileDecoder value
    in
    case decodedFile of
        Ok _ ->
            msg

        Err error ->
            BackendReturnedError (Json.Decode.errorToString error)


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


fileCount : Model -> Target -> String
fileCount model target =
    let
        filteredCount : Int
        filteredCount =
            List.Extra.count .satisfiesFilter <|
                case target of
                    Source ->
                        model.sourceFiles

                    Destination ->
                        model.destinationFiles

        totalCount : Int
        totalCount =
            List.length model.sourceFiles
    in
    if filteredCount == totalCount then
        String.fromInt filteredCount
            ++ inflect totalCount
            ++ " in directory"

    else
        String.fromInt filteredCount
            ++ " (on a total of "
            ++ String.fromInt totalCount
            ++ ")"
            ++ inflect totalCount
            ++ " in directory"


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


filterByName : List String -> File -> Bool
filterByName filters file =
    if file.name == ".." then
        True

    else
        let
            lowerCaseFilename : String
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
            lowerCaseParentPath : String
            lowerCaseParentPath =
                String.toLower file.parentPath
        in
        List.all (\word -> String.contains (String.toLower word) lowerCaseParentPath) filters



{- navigate t o the previous dir in history -}


filterDestinationFiles : Model -> Model
filterDestinationFiles model =
    let
        words : List String
        words =
            String.words model.destinationFilesFilter
    in
    case words of
        [] ->
            { model
                | destinationFiles = List.map (\f -> { f | satisfiesFilter = True }) model.destinationFiles
            }

        _ ->
            { model
                | destinationFiles =
                    List.map (\f -> { f | satisfiesFilter = filterByName words f }) model.destinationFiles
            }


filterSelectedFiles : List File -> List File
filterSelectedFiles files =
    List.filter (\f -> f.satisfiesFilter && f.status == Selected) files



-- TODO allow renaming destination files


filterSourceFiles : Model -> Model
filterSourceFiles model =
    let
        words : List String
        words =
            String.words model.sourceFilter
    in
    case words of
        [] ->
            { model
                | sourceFiles = List.map (\f -> { f | satisfiesFilter = True }) model.sourceFiles
            }

        _ ->
            { model
                | sourceFiles =
                    List.map (\f -> { f | satisfiesFilter = filterByName words f }) model.sourceFiles
            }


{-| Sets focus on an HTML element, then sends a msg when done (even if the element is not found)
-}
focusOn : String -> msg -> Cmd msg
focusOn elementId msg =
    Browser.Dom.focus elementId |> Task.attempt (\_ -> msg)


goBack : Model -> Target -> ( Model, Cmd Msg )
goBack model target =
    case target of
        Source ->
            let
                previousDir : String
                previousDir =
                    model.sourceNavigationHistory
                        |> List.head
                        |> Maybe.withDefault ""
            in
            if String.isEmpty previousDir then
                ( model, Cmd.none )

            else
                let
                    history : List String
                    history =
                        model.sourceNavigationHistory
                            |> List.drop 1
                in
                ( { model | sourceDirectoryPath = previousDir, sourceNavigationHistory = history }
                , getSourceDirectoryContent previousDir
                )

        Destination ->
            let
                previousDir : String
                previousDir =
                    model.destinationNavigationHistory
                        |> List.head
                        |> Maybe.withDefault ""
            in
            if String.isEmpty previousDir then
                ( model, Cmd.none )

            else
                let
                    history : List String
                    history =
                        model.destinationNavigationHistory
                            |> List.drop 1
                in
                ( { model | destinationDirectoryPath = previousDir, destinationNavigationHistory = history }
                , Cmd.batch
                    [ getDestinationSubdirectories previousDir
                    , getDestinationDirectoryFiles previousDir
                    ]
                )


highlight : String -> String -> List (Html Msg)
highlight =
    Mark.markWith { defaultOptions | whitespace = multiWord, minTermLength = 1 }


inflect : Int -> String
inflect count =
    if count > 1 then
        " files"

    else
        " file"


init : () -> ( Model, Cmd Msg )
init _ =
    ( defaultModel
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , getCurrentDirectoryPath ()
        ]
    )


isFileNotFoundError : Model -> Target -> String -> Bool
isFileNotFoundError model target errorMsg =
    let
        dir : String
        dir =
            case target of
                Source ->
                    model.sourceDirectoryPath

                Destination ->
                    model.destinationDirectoryPath
    in
    errorMsg == "open " ++ dir ++ ": no such file or directory"


keyDecoder : Json.Decode.Decoder Msg
keyDecoder =
    decodeKeyboardEvent
        |> Json.Decode.map
            (\key ->
                UserPressedOrReleasedKey key
            )


keyDecoderPreventingDefault : Target -> Json.Decode.Decoder ( Msg, Bool )
keyDecoderPreventingDefault target =
    decodeKeyboardEvent
        |> Json.Decode.map
            (\key ->
                ( UserPressedKey target key, True )
            )


matches : Model -> String -> String
matches model string =
    let
        maybeRegex : Maybe Regex.Regex
        maybeRegex =
            Pattern.fromString model.sourceSearch
                |> Pattern.toRegexp
    in
    case maybeRegex of
        Just regex ->
            Regex.find regex string
                |> List.map .match
                |> String.join " "

        Nothing ->
            ""


maxVisibleCharactersInPaths : Int
maxVisibleCharactersInPaths =
    50


moveSelectedFiles : Model -> ( Model, Cmd Msg )
moveSelectedFiles model =
    let
        ( filesToMove, destination ) =
            case model.focusedZone of
                RightSide ->
                    ( model.destinationFiles
                        |> List.filter (\f -> f.status == Selected)
                        |> List.map (\file -> file.parentPath ++ model.pathSeparator ++ file.name)
                    , model.sourceDirectoryPath
                    )

                _ ->
                    ( model.sourceFiles
                        |> filterSelectedFiles
                        |> List.map (\file -> file.parentPath ++ model.pathSeparator ++ file.name)
                    , model.destinationDirectoryPath
                    )
    in
    ( model
    , moveFiles ( filesToMove, destination )
    )


nameReplacement : Model -> File -> Maybe Renaming
nameReplacement model file =
    let
        newName : String
        newName =
            replace model file.name
    in
    if file.satisfiesFilter && (newName /= file.name) then
        Just
            { file = { file | name = newName }

            -- TODO prepend with the source dir?
            , originalPath = file.name
            }

    else
        Nothing


openSelectedFile : Model -> Target -> ( Model, Cmd Msg )
openSelectedFile model target =
    let
        fileToOpen : Maybe File
        fileToOpen =
            case target of
                Source ->
                    model.sourceFiles
                        |> List.Extra.find (\f -> f.satisfiesFilter && f.status == Selected)

                Destination ->
                    model.destinationFiles
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
        folderToOpen : String
        folderToOpen =
            case target of
                Source ->
                    model.sourceDirectoryPath

                Destination ->
                    model.destinationDirectoryPath
    in
    ( model, openFile folderToOpen )


parentDir : Model -> String -> String
parentDir model path =
    let
        index : Int
        index =
            String.indexes model.pathSeparator path
                |> List.Extra.last
                |> Maybe.withDefault (String.length path)
    in
    String.slice 0 index path


pathElementsRecursive : Model -> List File -> String -> List File
pathElementsRecursive model acc path =
    let
        endIndex : Int
        endIndex =
            String.length path

        file : File
        file =
            defaultDir
                |> withName leaf
                |> withParentPath parentPath

        lastPartIndex : Int
        lastPartIndex =
            1
                + (String.indexes model.pathSeparator path
                    |> List.maximum
                    |> Maybe.withDefault endIndex
                  )

        leaf : String
        leaf =
            String.slice lastPartIndex endIndex path

        parentPath : String
        parentPath =
            parentDir model path
    in
    if String.length parentPath > 0 then
        file
            :: pathElementsRecursive model acc parentPath

    else
        file
            :: acc


prepareSelectedFilesForRemoval : Model -> ( Model, Cmd Msg )
prepareSelectedFilesForRemoval model =
    case model.focusedZone of
        LeftSide ->
            ( { model
                | filesToDelete = filterSelectedFiles model.sourceFiles
                , focusedZone = Confirmation
                , previousFocusedZone = model.focusedZone
              }
                |> changeStatusOfSourceFiles Selected SelectedForDeletion
            , focusOn "delete-button" NoOp
            )

        RightSide ->
            ( { model
                | filesToDelete =
                    model.destinationFiles
                        |> List.filter (\f -> f.status == Selected)
                , focusedZone = Confirmation
                , previousFocusedZone = model.focusedZone
              }
                |> changeStatusOfDestinationFiles Selected SelectedForDeletion
            , focusOn "delete-button" NoOp
            )

        _ ->
            ( model, Cmd.none )


processConfirmationShortcuts : Model -> KeyboardEvent -> ( Model, Cmd Msg )
processConfirmationShortcuts model event =
    case ( event.keyCode, event.ctrlKey, event.metaKey ) of
        ( Key.Escape, False, False ) ->
            ( { model | filesToDelete = [], focusedZone = LeftSide }
                |> unmarkFilesForDeletion
            , focusOn "container-left" NoOp
            )

        ( Key.Enter, False, False ) ->
            removeSelectedFiles model

        _ ->
            ( model, Cmd.none )


processKeyboardShortcut : Model -> Target -> KeyboardEvent -> ( Model, Cmd Msg )
processKeyboardShortcut model target event =
    case model.focusedZone of
        Confirmation ->
            processConfirmationShortcuts model event

        LeftSide ->
            processMainShortcuts model target event

        FileNameEditor ->
            case event.keyCode of
                Key.Escape ->
                    let
                        sourceDirectoryFiles : List File
                        sourceDirectoryFiles =
                            model.sourceFiles
                                |> List.Extra.updateIf (\f -> f.status == Edited) (\f -> { f | status = Selected })
                    in
                    { model
                        | editedFile = Nothing
                        , editedFileName = ""
                        , sourceFiles = sourceDirectoryFiles
                    }
                        |> restoreFocus

                _ ->
                    ( model, Cmd.none )

        DirNameEditor ->
            case event.keyCode of
                Key.Escape ->
                    { model | editedDirName = "", isCreatingDirectory = False }
                        |> restoreFocus

                _ ->
                    ( model, Cmd.none )

        RightSide ->
            processMainShortcuts model target event

        _ ->
            ( model, Cmd.none )


processMainShortcuts : Model -> Target -> KeyboardEvent -> ( Model, Cmd Msg )
processMainShortcuts model target event =
    if event.ctrlKey || event.metaKey then
        case ( event.keyCode, event.shiftKey ) of
            ( Key.A, True ) ->
                changeAllFileStatus model target Unselected

            ( Key.A, False ) ->
                changeAllFileStatus model target Selected

            ( Key.R, False ) ->
                reload model target

            ( Key.Z, False ) ->
                undo model

            ( Key.Backspace, False ) ->
                prepareSelectedFilesForRemoval model

            _ ->
                ( model, Cmd.none )

    else if event.altKey then
        case event.keyCode of
            Key.F4 ->
                ( model, quit () )

            _ ->
                ( model, Cmd.none )

    else
        case ( event.keyCode, event.shiftKey ) of
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

            ( Key.U, False ) ->
                undo model

            ( Key.Left, _ ) ->
                goBack model target

            ( Key.Delete, False ) ->
                prepareSelectedFilesForRemoval model

            ( Key.F2, False ) ->
                renameSelectedSourceFile model

            ( Key.F5, False ) ->
                reload model target

            _ ->
                ( model, Cmd.none )


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


removeSelectedFiles : Model -> ( Model, Cmd Msg )
removeSelectedFiles model =
    let
        commands : List (Cmd msg)
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



-- SUBSCRIPTIONS


renameSelectedSourceFile : Model -> ( Model, Cmd Msg )
renameSelectedSourceFile model =
    let
        fileToEdit : Maybe File
        fileToEdit =
            model.sourceFiles
                |> List.Extra.find (\f -> f.satisfiesFilter && f.status == Selected)
    in
    case fileToEdit of
        Just file ->
            let
                sourceDirectoryFiles : List File
                sourceDirectoryFiles =
                    model.sourceFiles
                        |> List.Extra.updateIf (\f -> f == file) (\f -> { f | status = Edited })
            in
            ( { model
                | editedFile = Just file
                , editedFileName = file.name
                , sourceFiles = sourceDirectoryFiles
              }
            , focusOn "filename-input" NoOp
            )

        Nothing ->
            ( model, Cmd.none )


type alias Renaming =
    { file : File
    , originalPath : String
    }


replace : Model -> String -> String
replace model originalString =
    let
        maybeRegex : Maybe Regex.Regex
        maybeRegex =
            Pattern.fromString model.sourceSearch
                |> Pattern.toRegexp
    in
    case maybeRegex of
        Just regex ->
            Regex.replace regex (\_ -> model.sourceReplace) originalString

        Nothing ->
            originalString


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



-- VIEW


simpleKeyDecoder : Target -> Json.Decode.Decoder Msg
simpleKeyDecoder target =
    decodeKeyboardEvent
        |> Json.Decode.map
            (\key ->
                UserPressedKey target key
            )


sortByName : List { a | name : String } -> List { a | name : String }
sortByName =
    List.sortBy (.name >> String.toLower)


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


synchronizeDestinationDirFilter : Model -> Model
synchronizeDestinationDirFilter model =
    if model.applySourceFilterToDestinationDirectories then
        { model | destinationDirectoryFilter = model.sourceFilter }
            |> filterDestinationDirectories

    else
        model


synchronizeDestinationFilesFilter : Model -> Model
synchronizeDestinationFilesFilter model =
    if model.applySourceFilterToDestinationFiles then
        { model | destinationFilesFilter = model.sourceFilter }
            |> filterDestinationFiles

    else
        model


type Target
    = Source
    | Destination


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
        destination : String
        destination =
            command.source
                |> Maybe.withDefault ""

        filesToMove : List String
        filesToMove =
            command.files
                |> List.map (\f -> source ++ model.pathSeparator ++ f.name)

        source : String
        source =
            command.destination
                |> Maybe.withDefault ""
    in
    ( { model | isUndoing = True }
    , moveFiles ( filesToMove, destination ) :: cmds
    )


undoRenaming : Model -> List (Cmd Msg) -> Command -> ( Model, List (Cmd Msg) )
undoRenaming model cmds command =
    let
        encodedValue : Json.Encode.Value
        encodedValue =
            Json.Encode.object
                [ ( "oldName", Json.Encode.string oldName )
                , ( "newName", Json.Encode.string newName )
                ]

        newName : String
        newName =
            command.source |> Maybe.withDefault ""

        oldName : String
        oldName =
            command.destination |> Maybe.withDefault ""
    in
    ( { model
        | isUndoing = True
      }
    , renameFiles [ encodedValue ] :: cmds
    )


unixPathSep : String
unixPathSep =
    "/"


unmarkFilesForDeletion : Model -> Model
unmarkFilesForDeletion model =
    model
        |> changeStatusOfDestinationFiles SelectedForDeletion Selected
        |> changeStatusOfSourceFiles SelectedForDeletion Selected


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AdjustTimeZone newZone ->
            ( { model | timezone = newZone }
            , Cmd.none
            )

        BackendReturnedCreatedDirectory dir ->
            let
                newDirPath : String
                newDirPath =
                    model.destinationDirectoryPath ++ model.pathSeparator ++ dir.name
            in
            ( { model
                | editedDirName = ""
                , focusedZone = RightSide
                , isCreatingDirectory = False
                , previousFocusedZone = model.focusedZone
              }
                |> changeDestinationDirectory newDirPath
            , Cmd.batch
                [ getDestinationSubdirectories newDirPath
                , focusOn "container-right" NoOp
                ]
            )

        BackendReturnedCurrentDirPath path ->
            let
                pathSeparator : String
                pathSeparator =
                    if String.contains windowsPathSep path then
                        windowsPathSep

                    else
                        unixPathSep
            in
            ( { model
                | destinationDirectoryPath = path
                , pathSeparator = pathSeparator
                , sourceDirectoryPath = path
              }
            , Cmd.batch
                [ getSourceDirectoryContent path
                , getDestinationSubdirectories path
                , getDestinationDirectoryFiles path
                ]
            )

        BackendReturnedDestinationDirectories files ->
            ( { model | destinationSubdirectories = files |> sortByName }
                |> filterDestinationDirectories
            , Cmd.none
            )

        BackendReturnedDestinationDirectoryPath path ->
            if path == "" then
                -- user canceled the selection
                ( model, Cmd.none )

            else
                ( model |> changeDestinationDirectory path
                , Cmd.batch
                    [ getDestinationSubdirectories path
                    , getDestinationDirectoryFiles path
                    ]
                )

        BackendReturnedDestinationFiles fileList ->
            ( { model
                | destinationFiles = fileList |> sortByName
              }
                |> filterDestinationFiles
            , Cmd.none
            )

        BackendReturnedError errorMsg ->
            let
                ( updatedModel, cmd ) =
                    if isFileNotFoundError model Source errorMsg then
                        goBack model Source

                    else if isFileNotFoundError model Destination errorMsg then
                        goBack model Destination

                    else
                        ( model, Cmd.none )
            in
            ( { updatedModel
                | error = Just errorMsg
                , focusedZone = ErrorMessage
                , previousFocusedZone = model.focusedZone
              }
            , Cmd.batch
                [ focusOn "close-error" NoOp
                , cmd
                ]
            )

        BackendReturnedMovedFiles files ->
            ( if model.isUndoing then
                -- don't add anything to history
                { model | isUndoing = False }

              else
                let
                    destination : String
                    destination =
                        case firstMovedFile of
                            Just file ->
                                file.parentPath

                            Nothing ->
                                ""

                    firstMovedFile : Maybe File
                    firstMovedFile =
                        List.head files

                    source : String
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

        BackendReturnedRemovedFile ->
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
                                    newFile : File
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
                    | editedFile = Nothing
                    , editedFileName = ""
                    , focusedZone = LeftSide
                    , history = commands :: model.history
                    , previousFocusedZone = model.focusedZone
                    , sourceFiles =
                        List.map
                            (\f ->
                                if f.status == Edited then
                                    { f | status = Selected }

                                else
                                    f
                            )
                            model.sourceFiles
                }
            , Cmd.batch
                [ getSourceDirectoryContent model.sourceDirectoryPath
                , focusOn "container-left" NoOp
                ]
            )

        BackendReturnedSourceDirectoryContent directoryContent ->
            let
                ( dirList, fileList ) =
                    List.partition .isDir directoryContent
            in
            ( { model
                | sourceFiles = fileList |> sortByName
                , sourceSubDirectories = dirList |> sortByName
              }
                |> filterSourceFiles
            , Cmd.none
            )

        BackendReturnedSourceDirectoryPath path ->
            if path == "" then
                -- user canceled the selection
                ( model, Cmd.none )

            else
                ( model |> changeSourceDirectory path
                , getSourceDirectoryContent path
                )

        NoOp ->
            ( model, Cmd.none )

        UserChangedDestinationDirectoryFilter filteringString ->
            ( { model | destinationDirectoryFilter = filteringString }
                |> filterDestinationDirectories
            , Cmd.none
            )

        UserChangedDestinationFilesFilter filteringString ->
            ( { model | destinationFilesFilter = filteringString }
                |> filterDestinationFiles
            , Cmd.none
            )

        UserChangedSourceFilter filteringString ->
            ( { model | sourceFilter = filteringString }
                |> filterSourceFiles
                |> synchronizeDestinationDirFilter
                |> synchronizeDestinationFilesFilter
            , Cmd.none
            )

        UserChangedSourceReplace replaceString ->
            ( { model | sourceReplace = replaceString }
            , Cmd.none
            )

        UserChangedSourceSearch searchString ->
            ( { model | sourceSearch = searchString }, Cmd.none )

        UserClickedCancel ->
            let
                unselectForDeletion : File -> File
                unselectForDeletion file =
                    if file.status == SelectedForDeletion then
                        { file | status = Selected }

                    else
                        file
            in
            ( { model
                | destinationFiles = List.map unselectForDeletion model.destinationFiles
                , filesToDelete = []
                , focusedZone = LeftSide
                , previousFocusedZone = model.focusedZone
                , sourceFiles = List.map unselectForDeletion model.sourceFiles
              }
            , focusOn "container-left" NoOp
            )

        UserClickedClearSourceFilter ->
            ( { model | sourceFilter = "" }
                |> filterSourceFiles
                |> synchronizeDestinationDirFilter
            , Cmd.none
            )

        UserClickedClearDestinationDirectoryFilter ->
            ( { model | destinationDirectoryFilter = "" } |> filterDestinationDirectories
            , Cmd.none
            )

        UserClickedClearDestinationFilesFilter ->
            ( { model | destinationFilesFilter = "" } |> filterDestinationFiles
            , Cmd.none
            )

        UserClickedCloseError ->
            { model
                | error = Nothing
            }
                |> restoreFocus

        UserClickedDelete ->
            removeSelectedFiles model

        UserClickedDestinationDirectory isAbsolute file ->
            let
                newDestinationPath : String
                newDestinationPath =
                    case ( isAbsolute, file.name ) of
                        ( True, _ ) ->
                            file.parentPath ++ model.pathSeparator ++ file.name

                        ( False, ".." ) ->
                            parentDir model model.destinationDirectoryPath

                        ( False, _ ) ->
                            model.destinationDirectoryPath ++ model.pathSeparator ++ file.name
            in
            ( model |> changeDestinationDirectory newDestinationPath
            , Cmd.batch
                [ getDestinationDirectoryFiles newDestinationPath
                , getDestinationSubdirectories newDestinationPath
                ]
            )

        UserClickedDestinationDirectoryButton ->
            ( model, selectDestinationDirectory model.destinationDirectoryPath )

        UserClickedDestinationFile file ->
            let
                updatedDestinationFiles : List File
                updatedDestinationFiles =
                    select model model.destinationFiles file
            in
            ( { model | destinationFiles = updatedDestinationFiles }
            , Cmd.none
            )

        UserClickedReload target ->
            reload model target

        UserClickedReplaceButton ->
            let
                renamings : List Renaming
                renamings =
                    model.sourceFiles
                        |> List.filterMap (nameReplacement model)
            in
            ( { model | sourceReplace = "" }
            , applyRenaming model renamings
            )

        UserClickedSourceDirectory isAbsolute file ->
            let
                newSourcePath : String
                newSourcePath =
                    case ( isAbsolute, file.name ) of
                        ( True, _ ) ->
                            file.parentPath ++ model.pathSeparator ++ file.name

                        ( False, ".." ) ->
                            parentDir model model.sourceDirectoryPath

                        ( False, _ ) ->
                            model.sourceDirectoryPath ++ model.pathSeparator ++ file.name
            in
            ( model |> changeSourceDirectory newSourcePath
            , getSourceDirectoryContent newSourcePath
            )

        UserClickedSourceDirectoryButton ->
            ( model, selectSourceDirectory model.sourceDirectoryPath )

        UserClickedSourceFile file ->
            let
                updatedSourceFiles : List File
                updatedSourceFiles =
                    select model model.sourceFiles file
            in
            ( { model
                | focusedZone = LeftSide
                , previousFocusedZone = model.focusedZone
                , sourceFiles = updatedSourceFiles
              }
                |> filterSourceFiles
            , focusOn "container-left" NoOp
            )

        UserClickedSynchronizeDirFilterButton ->
            ( { model | applySourceFilterToDestinationDirectories = not model.applySourceFilterToDestinationDirectories }
                |> synchronizeDestinationDirFilter
            , Cmd.none
            )

        UserClickedSynchronizeFileFilterButton ->
            ( { model | applySourceFilterToDestinationFiles = not model.applySourceFilterToDestinationFiles }
                |> synchronizeDestinationFilesFilter
            , Cmd.none
            )

        UserChangedFocusedZone focus ->
            ( { model
                | focusedZone = focus
                , previousFocusedZone = model.focusedZone
              }
            , Cmd.none
            )

        UserModifiedFileName newName ->
            ( { model | editedFileName = newName }, Cmd.none )

        UserModifiedDirName newName ->
            ( { model | editedDirName = newName }, Cmd.none )

        UserPressedKey target event ->
            if model.isUndoing then
                -- ignore key presses while an undoing is being performed
                -- it could mess with the history
                ( model, Cmd.none )

            else
                processKeyboardShortcut model target event

        UserPressedOrReleasedKey keyboardEvent ->
            ( { model
                | isShiftPressed = keyboardEvent.shiftKey
                , isControlPressed = keyboardEvent.ctrlKey || keyboardEvent.metaKey
              }
            , Cmd.none
            )

        UserSubmittedDirName ->
            if model.isCreatingDirectory && model.editedDirName /= "" then
                createNewDirectory model

            else
                ( model, Cmd.none )

        UserSubmittedFilename ->
            let
                isConflicting : Bool
                isConflicting =
                    List.any (\f -> f.name == model.editedFileName) model.sourceFiles

                isNameEmpty : Bool
                isNameEmpty =
                    String.isEmpty model.editedFileName
            in
            case ( model.editedFile, isConflicting, isNameEmpty ) of
                ( Just _, True, False ) ->
                    ( { model
                        | error = Just ("A file with the name " ++ model.editedFileName ++ " already exists in the source directory")
                        , focusedZone = ErrorMessage
                      }
                    , focusOn "close-error" NoOp
                    )

                ( Just file, False, False ) ->
                    let
                        renaming : Renaming
                        renaming =
                            { file = { file | name = model.editedFileName }
                            , originalPath = file.name
                            }
                    in
                    ( { model | error = Nothing }
                    , applyRenaming model [ renaming ]
                    )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "app"
        , id "app"
        , Events.on "keydown" keyDecoder
        , Events.on "keyup" keyDecoder
        ]
        [ viewLeftSide model
        , viewRightSide model
        , viewFooter model
        ]


viewDate : Model -> Time.Posix -> Html Msg
viewDate model time =
    let
        day : String
        day =
            String.fromInt (Time.toDay model.timezone time)
                |> String.padLeft 2 '0'

        month : String
        month =
            Time.toMonth model.timezone time
                |> monthToString

        monthToString : Month -> String
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

        year : String
        year =
            String.fromInt (Time.toYear model.timezone time)
    in
    text (day ++ "/" ++ month ++ "/" ++ year)


viewDestination : Model -> List (Html Msg)
viewDestination model =
    [ viewDestinationSubdirectories model
    , viewDestinationFiles model
    ]


viewDestinationDirectoryFilter : Model -> Html Msg
viewDestinationDirectoryFilter model =
    div
        [ class "input-box"
        ]
        [ input
            [ class "input"
            , type_ "text"
            , id "filtering-dir-right"
            , onInput UserChangedDestinationDirectoryFilter
            , onFocus (UserChangedFocusedZone Filtering)
            , value model.destinationDirectoryFilter
            , placeholder "Enter one or more words to filter destination directories"
            , disabled model.applySourceFilterToDestinationDirectories
            ]
            []
        , button
            [ class "btn"
            , onClick UserClickedClearDestinationDirectoryFilter
            , disabled model.applySourceFilterToDestinationDirectories
            ]
            [ text "Clear" ]
        , button [ class "btn link", onClick UserClickedSynchronizeDirFilterButton ]
            [ if model.applySourceFilterToDestinationDirectories then
                text "Unlink"

              else
                text "Link"
            ]
        ]


viewDestinationFiles : Model -> Html Msg
viewDestinationFiles model =
    div [ class "panel" ]
        [ div [ class <| "panel-header" ++ additionalHeaderClass model RightSide ]
            [ h2 [] [ text <| fileCount model Destination ]
            ]
        , viewDestinationFilesFilter model
        , div
            [ class "panel-content" ]
            (model.destinationFiles
                |> List.filter .satisfiesFilter
                |> List.map (viewFile model UserClickedDestinationFile False)
            )
        ]


viewDestinationFilesFilter : Model -> Html Msg
viewDestinationFilesFilter model =
    div
        [ class "input-box"
        ]
        [ input
            [ class "input"
            , type_ "text"
            , id "filtering-files-right"
            , onInput UserChangedDestinationFilesFilter
            , onFocus (UserChangedFocusedZone Filtering)
            , value model.destinationFilesFilter
            , placeholder "Enter one or more words to filter files in destination directory"
            , disabled model.applySourceFilterToDestinationFiles
            ]
            []
        , button
            [ class "btn"
            , onClick UserClickedClearDestinationFilesFilter
            , disabled model.applySourceFilterToDestinationFiles
            ]
            [ text "Clear" ]
        , button [ class "btn link", onClick UserClickedSynchronizeFileFilterButton ]
            [ if model.applySourceFilterToDestinationFiles then
                text "Unlink"

              else
                text "Link"
            ]
        ]


viewDestinationSubdirectories : Model -> Html Msg
viewDestinationSubdirectories model =
    let
        newDirEditor : List (Html Msg)
        newDirEditor =
            if model.isCreatingDirectory then
                [ viewEditedDirectoryName model ]

            else
                []
    in
    div [ class "panel" ]
        [ div [ class <| "panel-header" ++ additionalHeaderClass model RightSide ]
            [ h2 []
                [ text <| "Destination: "
                , viewPath model Destination
                ]
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
        , viewDestinationDirectoryFilter model
        , div
            [ class "panel-content" ]
            (newDirEditor
                ++ (model.destinationSubdirectories
                        |> List.filter .satisfiesFilter
                        |> List.map (viewDirectory model (UserClickedDestinationDirectory False))
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


viewFile : Model -> (File -> Msg) -> Bool -> File -> Html Msg
viewFile model onClickMsg canBeSearchedAndReplaced file =
    if file.status == Edited then
        viewEditedFilename model

    else
        viewReadOnlyFile model onClickMsg canBeSearchedAndReplaced file


viewFooter : Model -> Html Msg
viewFooter model =
    let
        className : String
        className =
            if model.error /= Nothing || isWaitingForConfirmation then
                "danger"

            else
                ""

        conditionalAttributes : List (Html.Attribute Msg)
        conditionalAttributes =
            if isWaitingForConfirmation then
                [ Events.preventDefaultOn "keydown" (keyDecoderPreventingDefault Source)
                , onFocus (UserChangedFocusedZone Confirmation)
                ]

            else
                []

        isWaitingForConfirmation : Bool
        isWaitingForConfirmation =
            List.length model.filesToDelete > 0
    in
    footer
        (class className :: conditionalAttributes)
        [ case model.error of
            Just errorMsg ->
                div []
                    [ text errorMsg
                    , button [ id "close-error", class "btn", onClick UserClickedCloseError ] [ text "Ok" ]
                    ]

            Nothing ->
                viewPressedKeys model

        --        viewFocusedZone model
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


viewLeftSide : Model -> Html Msg
viewLeftSide model =
    let
        conditionalAttributes : List (Html.Attribute Msg)
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


viewPath : Model -> Target -> Html Msg
viewPath model target =
    let
        clickablePath : List (Html Msg)
        clickablePath =
            List.indexedMap
                (\index element ->
                    if index < displayableElementsCount - 1 then
                        span []
                            [ span
                                [ class "link"
                                , onClick <| msgToEmit element
                                ]
                                [ text element.name ]
                            , span [] [ text model.pathSeparator ]
                            ]

                    else
                        -- the name of the current directory is not clickable
                        span [] [ text element.name ]
                )
                displayableElements

        displayableElements : List File
        displayableElements =
            truncateConcatenatedNames maxVisibleCharactersInPaths elements

        displayableElementsCount : Int
        displayableElementsCount =
            List.length displayableElements

        elements : List File
        elements =
            pathElements model [] path

        ellipsis : Html Msg
        ellipsis =
            if displayableElementsCount < List.length elements then
                text <| "..." ++ model.pathSeparator

            else
                text model.pathSeparator

        msgToEmit : File -> Msg
        msgToEmit =
            case target of
                Source ->
                    UserClickedSourceDirectory True

                Destination ->
                    UserClickedDestinationDirectory True

        path : String
        path =
            case target of
                Source ->
                    model.sourceDirectoryPath

                Destination ->
                    model.destinationDirectoryPath
    in
    span [] <|
        ellipsis
            :: clickablePath


viewPressedKeys : Model -> Html Msg
viewPressedKeys model =
    text
        (if model.isControlPressed then
            " CTRL "

         else
            ""
                ++ (if model.isShiftPressed then
                        " SHIFT "

                    else
                        ""
                   )
        )


viewReadOnlyFile : Model -> (File -> Msg) -> Bool -> File -> Html Msg
viewReadOnlyFile model onClickMsg canBeSearchedAndReplaced file =
    let
        className : String
        className =
            case file.status of
                Unselected ->
                    "filename"

                Edited ->
                    "filename"

                Selected ->
                    "filename selected"

                SelectedForDeletion ->
                    "filename marked-for-deletion"

        fileName : List (Html Msg)
        fileName =
            if canBeSearchedAndReplaced then
                case ( model.sourceSearch, model.sourceReplace ) of
                    ( "", _ ) ->
                        [ text file.name ]

                    ( _, "" ) ->
                        highlight (matches model file.name) file.name

                    ( _, replacementString ) ->
                        file.name
                            |> replace model
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


viewRightSide : Model -> Html Msg
viewRightSide model =
    let
        conditionalAttributes : List (Html.Attribute Msg)
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


viewSourceFiles : Model -> Html Msg
viewSourceFiles model =
    div [ class "panel" ]
        [ div [ class <| "panel-header" ++ additionalHeaderClass model LeftSide ]
            [ h2 [] [ text <| fileCount model Source ]
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
        , div
            [ class "panel-content" ]
            (model.sourceFiles
                |> List.filter .satisfiesFilter
                |> List.map (viewFile model UserClickedSourceFile True)
            )
        ]


viewSourceSubdirectories : Model -> Html Msg
viewSourceSubdirectories model =
    div [ class "panel" ]
        [ div [ class <| "panel-header" ++ additionalHeaderClass model LeftSide ]
            [ h2 []
                [ text <| "Source: "
                , viewPath model Source
                ]
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
                |> List.map (viewDirectory model (UserClickedSourceDirectory False))
            )
        ]


windowsPathSep : String
windowsPathSep =
    "\\"
