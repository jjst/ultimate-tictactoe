module Tutorial exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html exposing (..)
import Html.App as App
import Markdown
import Maybe
import Array
import Keyboard

-- MODEL

type TutorialVisibility = Visible | Hidden

type alias Model = TutorialVisibility

init : (Model, Cmd Msg)
init = (Visible, Cmd.none)

-- UPDATE

type Msg = HideTutorial | NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  let
    newModel =
      case msg of
          HideTutorial -> Hidden
          NoOp -> model
  in
    newModel ! []

-- SUBSCRIPTIONS

escapeKey : Keyboard.KeyCode
escapeKey = 27

subscriptions : Model -> Sub Msg
subscriptions model = Keyboard.downs (\key -> if key == escapeKey then HideTutorial else NoOp)

-- VIEW

view : Model -> Html Msg
view model =
    case model of
        Hidden -> span [] []
        Visible ->
            let
                hideTutorialButton =
                    button [ style [ ("float", "right") ], onClick HideTutorial ]
                           [ text "Enough reading, let me play now!" ]
            in
                div [ class "tutorial" ] [ pageContent, hideTutorialButton ]

pageContent : Html msg
pageContent = Markdown.toHtml [class "content"] tutorialText


tutorialText =
    """
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
