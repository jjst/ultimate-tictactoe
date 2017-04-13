module Menu exposing (..)

import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html exposing (..)
import Markdown
import Maybe
import Array
import Keyboard


-- MODEL


type Model
    = NotChosen
    | OnePlayerVsAI
    | TwoPlayers


init : Model
init =
    NotChosen



-- UPDATE


type Msg
    = Choose1P
    | Choose2P


update : Msg -> Model -> Model
update msg model =
    case msg of
        Choose1P ->
            OnePlayerVsAI

        Choose2P ->
            TwoPlayers



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        NotChosen ->
            div [ class "tutorial" ]
                [ pageContent
                , div [ style [ ( "float", "right" ) ] ]
                    [ button [ onClick Choose1P ] [ text "1 Player vs AI" ]
                    , button [ onClick Choose2P ] [ text "2 Players" ]
                    ]
                ]

        _ ->
            span [] []


pageContent : Html msg
pageContent =
    Markdown.toHtml [ class "content" ] tutorialText


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
"""
