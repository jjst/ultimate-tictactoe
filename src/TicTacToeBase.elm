module TicTacToeBase exposing (..)

import Cell
import Player exposing (..)
import Board exposing (..)
import SvgUtils
import List as L
import Tuple3 as T3
import Html.Attributes
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Attributes as SA


type alias Color =
    String



-- MODEL


init : a -> Board a
init initialCell =
    T3.fill (T3.fill initialCell)



-- VIEW


grid : Int -> Svg a
grid cellSize =
    let
        offset =
            round ((toFloat cellSize) / 20)
    in
        g [ gridStyle ]
            [ line [ x1 (toString cellSize), y1 (toString offset), x2 (toString cellSize), y2 (toString (3 * cellSize - offset)) ] []
            , line [ x1 (toString (2 * cellSize)), y1 (toString offset), x2 (toString (2 * cellSize)), y2 (toString (3 * cellSize - offset)) ] []
            , line [ x1 (toString offset), y1 (toString cellSize), x2 (toString (3 * cellSize - offset)), y2 (toString cellSize) ] []
            , line [ x1 (toString offset), y1 (toString (2 * cellSize)), x2 (toString (3 * cellSize - offset)), y2 (toString (2 * cellSize)) ] []
            ]


strikeThrough : String -> Int -> Coords -> Coords -> Svg a
strikeThrough color cellSize ( i1, j1 ) ( i2, j2 ) =
    let
        toSvgCoords =
            \x offset -> (T3.toFloat x + 0.5 + offset) * (toFloat cellSize) |> toString
    in
        line
            [ SA.style ("stroke-width:10;stroke:" ++ color)
            , x1 (toSvgCoords i1 -0.05)
            , y1 (toSvgCoords j1 -0.05)
            , x2 (toSvgCoords i2 0.05)
            , y2 (toSvgCoords j2 0.05)
            ]
            []


gridStyle : Attribute msg
gridStyle =
    Svg.Attributes.style "stroke:black;stroke-width:4"


cellSize : Int
cellSize =
    220


boardSize : Int
boardSize =
    cellSize * 3


type alias SvgViewCell a b =
    Coords -> a -> Svg b


svgView : OwnerFunction a -> SvgViewCell a b -> Board a -> Svg b
svgView owner svgViewCell board =
    let
        cells =
            g [] (flatten <| (indexedMap svgViewCell board))

        st =
            case ( winningRow owner board, winner owner board ) of
                ( Just ( first, middle, last ), Just winner ) ->
                    case winner of
                        Left Player.O ->
                            [ strikeThrough "red" cellSize first last ]

                        Left Player.X ->
                            [ strikeThrough "blue" cellSize first last ]

                        _ ->
                            []

                _ ->
                    []
    in
        g [] ([ cells, (grid cellSize) ] ++ st)
