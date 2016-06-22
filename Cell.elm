module Cell exposing (Model, Msg, init, update, svgView, view)

import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)

import Player exposing (..)


main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

-- MODEL

type alias Model =
    { mark : Maybe Player
    , currentPlayer : Player
    }


init : Model
init = { mark = Nothing, currentPlayer = X }

-- UPDATE

type Msg = PlaceMark

update : Msg -> Model -> Model
update msg ({mark, currentPlayer} as model) =
    case msg of
        PlaceMark ->
            case mark of
                Just _ -> model
                Nothing -> Model (Just currentPlayer) (opponent currentPlayer)


-- VIEW

svgView : Model -> Svg Msg
svgView {mark, currentPlayer} =
    let
        markDrawing = case mark of
            Nothing -> []
            Just X -> [drawCross]
            Just O -> [drawCircle]
    in
          g []
          ([ rect [ x "0", y "0", width "100", height "100", fillOpacity "0.0", onClick PlaceMark ] []
          ] ++ markDrawing)


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 100 100", width "300px", transform "scale(0.3)" ]
        [(svgView model)]

drawCircle : Svg a
drawCircle = circle [ cx "50", cy "50", r "45", fillOpacity "0.0", markStyle ] []

drawCross : Svg a
drawCross =
    g []
      [ line [ x1 "5", y1 "5", x2 "95", y2 "95", markStyle ] []
      , line [ x1 "5", y1 "95", x2 "95", y2 "5", markStyle ] []
      ]

markStyle : Attribute msg
markStyle =
    Svg.Attributes.style "stroke:black;stroke-width:4"

