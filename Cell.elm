-- module Cell

import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
main = 
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

-- MODEL

type Player
    = X
    | O

type alias Model = 
    { mark : Maybe Player
    , nextPlayer : Player
    }


init : Model
init = { mark = Nothing, nextPlayer = X }

opponent : Player -> Player
opponent player =
    case player of
        X -> O
        O -> X

-- UPDATE

type Msg
    = FillX
    | FillO

update : Msg -> Model -> Model
update msg ({mark, nextPlayer} as model) =
    case mark of
        Just _ -> model
        Nothing -> 
            case msg of
                FillX -> Model (Just X) (opponent nextPlayer)
                FillO -> Model (Just O) (opponent nextPlayer)


-- VIEW

svgView : Model -> Svg Msg
svgView {mark, nextPlayer} =
    let
        msg = case nextPlayer of
           X -> FillX
           O -> FillO 
        markDrawing = case mark of
            Nothing -> []
            Just X -> [drawCross]
            Just O -> [drawCircle]
    in
          g []
          ([ rect [ x "0", y "0", width "100", height "100", fill "#0B79CE", onClick msg] [] 
          ] ++ markDrawing)


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 100 100", width "300px", transform "scale(0.3)" ]
        [(svgView model)]

helloSvg : Svg a
helloSvg = Svg.text "hello"

drawCircle : Svg a
drawCircle = circle [ cx "50", cy "50", r "45", fill "#9B79CE", markStyle ] [] 

drawCross : Svg a
drawCross =
    g []
      [ line [ x1 "5", y1 "5", x2 "95", y2 "95", markStyle ] []
      , line [ x1 "5", y1 "95", x2 "95", y2 "5", markStyle ] []
      ]

markStyle : Attribute msg
markStyle = 
    Svg.Attributes.style "stroke:black;stroke-width:4"

