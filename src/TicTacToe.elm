module TicTacToe exposing (Move, TicTacToeBoard, fromString, init, performMoveFor, render, winner)

import Board exposing (Board)
import Cell
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import List as L
import Player exposing (..)
import Sizes
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SvgUtils
import TicTacToeBase
import Tuple3 as T3


type alias TicTacToeBoard =
    Board Cell.Cell


type alias Move =
    Board.Coords



-- MODEL


init : TicTacToeBoard
init =
    TicTacToeBase.init Nothing


winner : TicTacToeBoard -> Maybe Winner
winner =
    Board.winner identity


fromString : String -> Result String TicTacToeBoard
fromString str =
    let
        liftResult : List (Result a b) -> Result a (List b)
        liftResult list =
            list |> L.foldr (\result listResult -> listResult |> Result.andThen (\items -> Result.map (\a -> (::) a items) result)) (Ok [])

        parseLine : String -> Result String (List Cell.Cell)
        parseLine l =
            l
                |> String.trim
                |> String.split " "
                |> L.map Cell.fromString
                |> liftResult

        parsed : Result String (List (List Cell.Cell))
        parsed =
            str
                |> String.trim
                |> String.lines
                |> L.map parseLine
                |> liftResult

        boardResult =
            Result.map
                (L.map
                    (T3.fromList >> Result.fromMaybe "Wrong number of items in row")
                )
                parsed
                |> Result.andThen liftResult
                |> Result.andThen (T3.fromList >> Result.fromMaybe "Wrong number of rows")
    in
    boardResult



-- UPDATE


performMoveFor : Player -> Move -> TicTacToeBoard -> TicTacToeBoard
performMoveFor player ( x, y ) board =
    case winner board of
        Just _ ->
            board

        Nothing ->
            board
                |> Board.indexedMap
                    (\( i, j ) cell ->
                        if ( x, y ) == ( i, j ) then
                            Just player

                        else
                            cell
                    )



-- VIEW


render : TicTacToeBoard -> Svg Move
render =
    TicTacToeBase.svgView identity svgViewCell


svgViewCell : Board.Coords -> Cell.Cell -> Svg Move
svgViewCell ( i, j ) model =
    Cell.svgView model ( i, j )
        |> SvgUtils.scale (toFloat Sizes.cellSize / 100.0)
        |> SvgUtils.translate (toFloat (T3.toInt i * Sizes.cellSize)) (toFloat (T3.toInt j * Sizes.cellSize))
