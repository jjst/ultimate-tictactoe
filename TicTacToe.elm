import Cell
import Player exposing (..)
import SvgUtils

import List as L
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

type alias Model =
    { board : List (List Cell.Model)
    , currentPlayer : Player
    }


init : Model
init =
    { board = List.repeat 3 (List.repeat 3 Cell.init)
    , currentPlayer = X
    }

mapWithIndex : ((Int,Int) -> a -> b) -> List (List a) -> List (List b)
mapWithIndex f matrix =
    L.indexedMap (\i row -> L.indexedMap (\j val -> f (i,j) val) row) matrix


-- UPDATE

type Msg = PlaceMark Int Int Cell.Msg

update : Msg -> Model -> Model
update msg ({board, currentPlayer} as model) =
    let
        nextPlayer = opponent currentPlayer
    in
        case msg of
            PlaceMark x y msg ->
                { board = mapWithIndex (\(i,j) cell ->
                    if (x,y) == (i,j) then Cell.update msg cell else { cell | currentPlayer = nextPlayer }) board
                , currentPlayer = nextPlayer
                }


-- VIEW

view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 800 800", width "800px" ]
        [(svgView model)]

svgView : Model -> Svg Msg
svgView {board, currentPlayer} =
    let
        cells = g [] (L.concat <| mapWithIndex svgViewCell board)
    in
          g [] [ cells, grid ]

grid : Svg a
grid =
    g [ gridStyle ]
      [ line [ x1 "100", y1 "5", x2 "100", y2 "295" ] []
      , line [ x1 "200", y1 "5", x2 "200", y2 "295" ] []
      , line [ x1 "5", y1 "100", x2 "295", y2 "100" ] []
      , line [ x1 "5", y1 "200", x2 "295", y2 "200" ] []
      ]

gridStyle : Attribute msg
gridStyle =
    Svg.Attributes.style "stroke:black;stroke-width:4"


svgViewCell : (Int, Int) -> Cell.Model -> Svg Msg
svgViewCell (i,j) model =
    App.map (PlaceMark i j) (SvgUtils.translate (i*100) (j*100) (Cell.svgView model))
