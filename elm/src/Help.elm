module Help exposing (view)

import Html exposing (Html, div, h1, h2, hr, kbd, p, section, text)
import Html.Attributes exposing (class)


type alias Section =
    { title : String
    , description : String
    , keymap : List Shortcut
    }


type alias Shortcut =
    { keys : List String
    , description : String
    }


help : List Section
help =
    [ { title = "Selection"
      , description = "Selects files in the focused zone (Source or Destination)"
      , keymap =
            [ { keys = [ "ARROW LEFT" ], description = "Navigate backwards" }
            , { keys = [ "ARROW RIGHT" ], description = "Navigate forward" }
            , { keys = [ "ARROW UP" ], description = "Select previous file (or first file in the list if none is selected)" }
            , { keys = [ "ARROW DOWN" ], description = "Select next file (or last file in the list if none is selected)" }
            , { keys = [ "SHIFT", "ARROW UP" ], description = "Extend selection upwards" }
            , { keys = [ "SHIFT", "ARROW DOWN" ], description = "Extend selection downwards)" }
            , { keys = [ "CTRL", "A" ], description = "Select All files" }
            , { keys = [ "CTRL", "SHIFT", "A" ], description = "Unselect All files" }
            ]
      }
    , { title = "Act on files"
      , description = "Applies to focused zone (source or destination)"
      , keymap =
            [ { keys = [ "CTRL", "Backspace" ], description = "Delete selected files" }
            , { keys = [ "DELETE" ], description = "Delete selected files" }
            , { keys = [ "F" ], description = "Display first selected file in finder (MacOS) or explorer (Windows)" }
            , { keys = [ "M" ], description = "Move selected files" }
            , { keys = [ "O" ], description = "Open first selected file in default application" }
            , { keys = [ "R" ], description = "Rename selected files" }
            , { keys = [ "F2" ], description = "Rename selected files" }
            , { keys = [ "CTRL", "R" ], description = "Reload file list" }
            , { keys = [ "F5" ], description = "Reload file list" }
            ]
      }
    , { title = "Filter"
      , description = "Filter files or directories"
      , keymap =
            [ { keys = [ "ALT", "D" ], description = "Focus Destination directories filter" }
            , { keys = [ "ALT", "F" ], description = "Focus Destination files filter" }
            , { keys = [ "ALT", "S" ], description = "Focus Source directories filter" }
            , { keys = [ "C" ], description = "Copy source filter to destination filters" }
            , { keys = [ "X" ], description = "Delete all filters" }
            ]
      }
    , { title = "Act on directories"
      , description = ""
      , keymap =
            [ { keys = [ "N" ], description = "Create sub-directory in current destination directory" }
            , { keys = [ "CTRL", "R" ], description = "Reload destination directories list" }
            , { keys = [ "F5" ], description = "Reload destination directories list" }
            ]
      }
    , { title = "Miscellaneous"
      , description = ""
      , keymap =
            [ { keys = [ "H" ], description = "Display this help" }
            , { keys = [ "ALT", "F4" ], description = "Quit" }
            , { keys = [ "CTRL", "Q" ], description = "Quit" }
            , { keys = [ "U" ], description = "Undo last action" }
            , { keys = [ "CTRL", "F" ], description = "Focus search field" }
            , { keys = [ "CTRL", "Z" ], description = "Undo last action" }
            , { keys = [ "ESC" ], description = "Cancel current edition (file renaming, directory creation)" }
            ]
      }
    ]


view : Html msg
view =
    div [ class "help-content" ] <|
        [ h1 [] [ text "Help - Keyboard shortcuts" ]
        , p [] [ text "On MacOS, CTRL and CMD maybe be used indifferently in keyboard shortcuts." ]
        ]
            ++ List.map viewSection help


viewSection : Section -> Html msg
viewSection helpSection =
    section [] <|
        [ h2 [] [ text helpSection.title ]
        , hr [] []
        , p [] [ text helpSection.description ]
        ]
            ++ List.map viewShortcut helpSection.keymap


viewShortcut : Shortcut -> Html msg
viewShortcut shortcut =
    div [ class "help-row" ]
        [ div [ class "keyboard-shortcut" ] <|
            List.map key shortcut.keys
        , div [ class "action" ] [ text shortcut.description ]
        ]


key : String -> Html msg
key str =
    kbd [] [ text str ]
