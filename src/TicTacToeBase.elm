module TicTacToeBase exposing (SvgViewCell, grid, gridStyle, init, strikeThrough, svgView)

import Board exposing (..)
import Cell
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import List as L
import Player exposing (..)
import Sizes
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)
import SvgUtils
import Tuple3 as T3



-- MODEL


init : a -> Board a
init initialCell =
    T3.fill (T3.fill initialCell)



-- VIEW


grid : Int -> Svg a
grid cellSize =
    let
        offset =
            round (toFloat cellSize / 20)
    in
    g [ gridStyle ]
        [ line [ x1 (String.fromInt cellSize), y1 (String.fromInt offset), x2 (String.fromInt cellSize), y2 (String.fromInt (3 * cellSize - offset)) ] []
        , line [ x1 (String.fromInt (2 * cellSize)), y1 (String.fromInt offset), x2 (String.fromInt (2 * cellSize)), y2 (String.fromInt (3 * cellSize - offset)) ] []
        , line [ x1 (String.fromInt offset), y1 (String.fromInt cellSize), x2 (String.fromInt (3 * cellSize - offset)), y2 (String.fromInt cellSize) ] []
        , line [ x1 (String.fromInt offset), y1 (String.fromInt (2 * cellSize)), x2 (String.fromInt (3 * cellSize - offset)), y2 (String.fromInt (2 * cellSize)) ] []
        ]


strikeThrough : String -> Int -> Coords -> Coords -> Svg a
strikeThrough color cellSize ( i1, j1 ) ( i2, j2 ) =
    let
        toSvgCoords =
            \x offset -> (T3.toFloat x + 0.5 + offset) * toFloat cellSize |> String.fromFloat
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
    SA.style "stroke:black;stroke-width:4"


type alias SvgViewCell a b =
    Coords -> a -> Svg b


svgView : OwnerFunction a -> SvgViewCell a b -> Board a -> Svg b
svgView owner svgViewCell board =
    let
        cells =
            g [] (flatten <| indexedMap svgViewCell board)

        st =
            case ( winningRow owner board, winner owner board ) of
                ( Just ( first, middle, last ), Just winner ) ->
                    case winner of
                        Left Player.O ->
                            [ strikeThrough "red" Sizes.cellSize first last ]

                        Left Player.X ->
                            [ strikeThrough "blue" Sizes.cellSize first last ]

                        _ ->
                            []

                _ ->
                    []
    in
    g [] ([ cells, grid Sizes.cellSize ] ++ st)
