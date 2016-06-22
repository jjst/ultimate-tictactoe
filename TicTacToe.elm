import Cell
import Player exposing (..)
import Board exposing (..)
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
    { board : Board
    , currentPlayer : Player
    }

init : Model
init =
    { board = List.repeat 3 (List.repeat 3 Cell.init)
    , currentPlayer = X
    }


-- UPDATE

type Msg = PlaceMark Int Int Cell.Msg

update : Msg -> Model -> Model
update msg ({board, currentPlayer} as model) =
    let
        nextPlayer = opponent currentPlayer
    in
       case winner board of
           Just _ -> model
           Nothing ->
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
        st = case (winningRow board) of
            Just [first,middle,last] -> [ strikeThrough first last ]
            _ -> []
    in
          g [] ([ cells, grid ] ++ st)

grid : Svg a
grid =
    g [ gridStyle ]
      [ line [ x1 "100", y1 "5", x2 "100", y2 "295" ] []
      , line [ x1 "200", y1 "5", x2 "200", y2 "295" ] []
      , line [ x1 "5", y1 "100", x2 "295", y2 "100" ] []
      , line [ x1 "5", y1 "200", x2 "295", y2 "200" ] []
      ]

strikeThrough: (Int,Int) -> (Int,Int) -> Svg a
strikeThrough (i1,j1) (i2,j2) =
    let
        toSvgCoords = \x offset -> (toFloat x + 0.5 + offset) * 100 |> toString
    in
        line [ strikeThroughStyle
             , x1 (toSvgCoords i1 -0.05)
             , y1 (toSvgCoords j1 -0.05)
             , x2 (toSvgCoords i2 0.05)
             , y2 (toSvgCoords j2 0.05)
             ] []


gridStyle : Attribute msg
gridStyle =
    Svg.Attributes.style "stroke:black;stroke-width:4"

strikeThroughStyle : Attribute msg
strikeThroughStyle =
    Svg.Attributes.style "stroke:red;stroke-width:7"


svgViewCell : (Int, Int) -> Cell.Model -> Svg Msg
svgViewCell (i,j) model =
    App.map (PlaceMark i j) (SvgUtils.translate (i*100) (j*100) (Cell.svgView model))
