module Tutorial exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html exposing (..)
import Html.App as App
import Markdown
import Maybe
import Array
import Keyboard



main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model = Maybe Int

init : (Model, Cmd Msg)
init = (Just 0, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.presses switchPage


switchPage : Keyboard.KeyCode -> Msg
switchPage keyPress =
    case keyPress of
      37 -> PreviousPage
      39 -> NextPage
      _  -> NoOp

-- UPDATE

type Msg
    = NextPage
    | PreviousPage
    | SkipTutorial
    | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel =
      case msg of
          NextPage -> Maybe.map (\ pageNumber -> if pageNumber < (Array.length pages - 1) then pageNumber + 1 else pageNumber ) model
          PreviousPage -> Maybe.map (\ pageNumber -> if pageNumber > 0 then pageNumber - 1 else pageNumber ) model
          SkipTutorial -> Nothing
          _ -> model
  in
    (newModel, Cmd.none)


-- VIEW

view : Model -> Html Msg
view pageNumber =
    case pageNumber of
            Nothing -> span [] []
            Just num ->
                let
                    finishButton = button [ style [ ("float", "right") ], onClick SkipTutorial ] [ text "Enough reading, let me play now!" ]
                    buttons = [ finishButton ]
                    content = pageContent num
                in
                    div [ class "tutorial" ] ([ content ] ++ buttons)

pageContent : Int -> Html msg
pageContent index =
   Array.get index pages |> Maybe.withDefault "" |> (Markdown.toHtml [class "content"])


pages = Array.fromList
    [ """
# This is Ultimate Tic-Tac-Toe
Ultimate Tic-Tac-Toe is a modern, funky variant of the venerable (but ultimately
<a href="https://xkcd.com/832/" target="_blank">dull and predictable</a>)
2-player Tic-Tac-Toe we all know.

In Ultimate Tic-Tac-Toe, each cell is divided into another Tic-Tac-Toe board. You have to win three cells in a row to
win the game. But there's a twist! You don't get to pick which board to play in: whichever _cell_ your opponent picks
determines the _board_ you must play in next.

If you've never played before, check out <a href="https://mathwithbaddrawings.com/2013/06/16/ultimate-tic-tac-toe/" target="_blank">this page</a>
for instructions before you get started. It's a short read, promise!

This is a 2-player game, so you will need to fetch you a human companion. A one player version is coming soon for people preferring
the company of robots.
"""
    ]
