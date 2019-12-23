module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (Html, button, div, text, p, ul, li, h1, h2)
import Html.Events exposing (onClick)


-- Main

root_dir = RootDirectory [ FILE (BasicFile "input.txt")
                         , FILE (HiddenFile ".gitignore")
                         , DIRECTORY (BasicDirectory "test" [ FILE (BasicFile "input.txt")
                                                            , FILE (HiddenFile ".gitignore")
                                                            ])
                         ]
init = { root = root_dir
       , cwd = root_dir
       }

main =
    Browser.sandbox { init = init, update = update, view = view }


-- Model


type alias Model = { root: DirectoryType, cwd: DirectoryType }

type File
    = FILE FileType
    | DIRECTORY DirectoryType

type FileType
    = BasicFile String
    | HiddenFile String

type DirectoryType
    = BasicDirectory String (List File)
    | HiddenDirectory String (List File)
    | RootDirectory (List File)


-- Update


type Msg
    = NotToDo

update msg model =
    case msg of
        NotToDo ->
            model


-- View


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Repository" ]
        , h2 [] [ text "ls" ]
        , displayLs root_dir
        , h2 [] [ text "tree" ]
        , displayTree root_dir
        ]


-- Functions


fileName : File -> String
fileName file =
    case file of
        FILE (BasicFile filename) -> filename
        FILE (HiddenFile filename) -> filename
        DIRECTORY (BasicDirectory dirname _) -> dirname
        DIRECTORY (HiddenDirectory dirname _) ->  dirname
        DIRECTORY (RootDirectory _) -> "/"

ls : DirectoryType -> List File
ls directory =
    case directory of
        BasicDirectory _ files -> files
        HiddenDirectory _ files -> files
        RootDirectory files -> files

displayLs : DirectoryType -> Html Msg
displayLs directory =
    ul [] <|
        List.map (\f -> li [] [ text (fileName f) ]) (ls directory)

displayTree : DirectoryType -> Html Msg
displayTree directory =
    ul [] <|
        List.map (\f -> displayTreeFile f) (ls directory)

displayTreeFile : File -> Html Msg
displayTreeFile file =
    case file of
        FILE _ -> li [] [ text (fileName file)]
        DIRECTORY directory -> li [] [ ul [] <| List.map (\f -> displayTreeFile f) (ls directory) ]
