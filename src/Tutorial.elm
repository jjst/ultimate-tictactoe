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
                    finishButton = button [ style [ ("float", "right") ], onClick SkipTutorial ] [ text "Got it!" ]
                    buttons = [ finishButton ]
                    content = pageContent num
                    tutorialStyle = style
                      [ ("border-radius", "25px")
                      , ("border", "2px solid #73AD21")
                      , ("background", "#EEEEEE")
                      , ("padding", "20px")
                      , ("overflow", "hidden")
                      , ("opacity", "0.95")
                      ]
                in
                    div [ tutorialStyle ] ([ content ] ++ buttons)

pageContent : Int -> Html msg
pageContent index =
   Array.get index pages |> Maybe.withDefault "" |> (Markdown.toHtml [class "content"])


pages = Array.fromList
    [ """
Ultimate Tic-Tac-Toe is a modern, funky twist on the venerable (but ultimately
[dull and predictable](https://xkcd.com/832/)) Tic-Tac-Toe we all know.
In Ultimate Tic-Tac-Toe, each cell is divided into another Tic-Tac-Toe grid.
Check out <a href="https://mathwithbaddrawings.com/2013/06/16/ultimate-tic-tac-toe/" target="_blank">this page</a> for
instructions on how to play.
"""
    ]
