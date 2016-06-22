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
    , nextPlayer : Player
    }


init : Model
init = 
    { board = List.repeat 3 (List.repeat 3 Cell.init)
    , nextPlayer = X 
    }

mapWithIndex : ((Int,Int) -> a -> b) -> List (List a) -> List (List b)
mapWithIndex f matrix =
    L.indexedMap (\i row -> L.indexedMap (\j val -> f (i,j) val) row) matrix


-- UPDATE

type Msg = PlaceMark Int Int Cell.Msg

update : Msg -> Model -> Model
update msg ({board, nextPlayer} as model) =
    let
        o = opponent nextPlayer
    in
        case msg of
            PlaceMark x y msg ->
                { board = mapWithIndex (\(i,j) cell ->
                    if (x,y) == (i,j) then Cell.update msg cell else { cell | nextPlayer = o }) board
                , nextPlayer = o
                }


-- VIEW

view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 800 800", width "800px" ]
        [(svgView model)]

svgView : Model -> Svg Msg
svgView {board, nextPlayer} =
    let
        cells = L.concat <| mapWithIndex svgViewCell board
    in
          g [] cells

svgViewCell : (Int, Int) -> Cell.Model -> Svg Msg
svgViewCell (i,j) model =
    App.map (PlaceMark i j) (SvgUtils.translate (i*100) (j*100) (Cell.svgView model))
