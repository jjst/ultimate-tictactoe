module TicTacToeBase exposing (..)

import Cell
import Player exposing (..)
import Board exposing (..)
import SvgUtils

import List as L
import Tuple3 as T3
import Html.Attributes
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


-- MODEL

type alias Model a =
    { board : Board a
    , currentPlayer : Player
    }

init : a -> Model a
init initialCell =
    { board = T3.fill (T3.fill initialCell)
    , currentPlayer = X
    }


-- VIEW

grid : Int -> Svg a
grid cellSize =
    let
        offset = round ((toFloat cellSize) / 20)
    in
        g [ gridStyle ]
          [ line [ x1 (toString cellSize), y1 (toString offset), x2 (toString cellSize), y2 (toString (3*cellSize - offset)) ] []
          , line [ x1 (toString (2*cellSize)), y1 (toString offset), x2 (toString (2*cellSize)), y2 (toString (3*cellSize - offset)) ] []
          , line [ x1 (toString offset), y1 (toString cellSize), x2 (toString (3*cellSize - offset)), y2 (toString cellSize) ] []
          , line [ x1 (toString offset), y1 (toString (2*cellSize)), x2 (toString (3*cellSize - offset)), y2 (toString (2*cellSize)) ] []
          ]

strikeThrough: Int -> Coords -> Coords -> Svg a
strikeThrough cellSize (i1,j1) (i2,j2) =
    let
        toSvgCoords = \x offset -> (T3.toFloat x + 0.5 + offset) * (toFloat cellSize) |> toString
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

cellSize : Int
cellSize = 220

boardSize : Int
boardSize = cellSize * 3

type alias SvgViewCell a b = (Coords -> a -> Svg b)

view : OwnerFunction a -> SvgViewCell a b -> Model a -> Html b
view owner svgViewCell model =
    let
        size = (toString boardSize)
        divStyle =
          Html.Attributes.style
            [ ("margin", "auto")
            , ("width", size ++ "px")
            ]
    in
        div [ divStyle ]
            [
            svg [ viewBox ("0 0 " ++ size ++ " " ++ size), width (size ++ "px") ]
                [(svgView owner svgViewCell model)]
            ]


svgView : OwnerFunction a -> SvgViewCell a b -> Model a -> Svg b
svgView owner svgViewCell model =
    let
        cells = g [] (flatten <| (indexedMap svgViewCell model.board))
        st = case (winningRow owner model.board) of
            Just (first,middle,last) -> [ strikeThrough cellSize first last ]
            _ -> []
    in
          g [] ([ cells, (grid cellSize) ] ++ st)

