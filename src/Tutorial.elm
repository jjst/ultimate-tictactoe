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
                    skipTutorialButton =
                      button [ style [ ("float", "right") ], onClick SkipTutorial ] [ text "Let me play already!" ]
                    buttons =
                        if num == 0 then
                           [ button [ style [ ("visibility", "hidden") ], onClick PreviousPage ] [ text "Previous" ]
                           , button [ onClick NextPage ] [ text "Next" ]
                           , skipTutorialButton
                           ]
                        else if num == Array.length pages - 1 then
                           [ button [ onClick PreviousPage ] [ text "Previous" ]
                           , button [ onClick SkipTutorial ] [ text "Play!" ]
                           ]
                        else
                           [ button [ onClick PreviousPage ] [ text "Previous" ]
                           , button [ onClick NextPage ] [ text "Next" ]
                           , skipTutorialButton
                           ]
                    content = pageContent num
                    tutorialStyle = style
                      [ ("border-radius", "25px")
                      , ("border", "2px solid #73AD21")
                      , ("background", "white")
                      , ("padding", "20px")
                      , ("overflow", "hidden")
                      ]
                in
                    div [ tutorialStyle ] ([ content ] ++ buttons)

pageContent : Int -> Html msg
pageContent index =
   Array.get index pages |> Maybe.withDefault "" |> (Markdown.toHtml [class "content"])


pages = Array.fromList
    [ """
# Apple Pie Recipe

  1. Invent the universe.
  2. Bake an apple pie.

"""
    , """
# Ouiche Recipe

  1. Invent the universe.
  2. Bake a ouiche.

""" , """
# Hummus Recipe

  1. Invent the universe.
  2. Bake hummus.

"""
    ]
