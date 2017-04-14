module TicTacToe exposing (..)

import Cell
import Player exposing (..)
import Board exposing (Board)
import Board
import SvgUtils
import TicTacToeBase
import String
import List as L
import Tuple3 as T3
import Html.Attributes
import Html
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias TicTacToeBoard =
    Board Cell.Cell



-- MODEL


type alias Model =
    TicTacToeBase.Model Cell.Cell


init : Model
init =
    TicTacToeBase.init Nothing


winner : TicTacToeBoard -> Maybe Winner
winner =
    Board.winner identity


fromString : Player -> String -> Result String Model
fromString player str =
    let
        liftResult : List (Result a b) -> Result a (List b)
        liftResult list =
            list |> L.foldr (\result listResult -> listResult |> Result.andThen (\list -> Result.map (flip (::) list) result)) (Ok [])

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
        boardResult |> Result.map (\b -> { board = b, currentPlayer = player })



-- UPDATE


type Msg
    = PlaceMark Board.Coords
    | TogglePlayer


update : Msg -> Model -> Model
update msg ({ board, currentPlayer } as model) =
    let
        nextPlayer =
            opponent currentPlayer
    in
        case winner board of
            Just _ ->
                model

            Nothing ->
                case msg of
                    PlaceMark ( x, y ) ->
                        { board =
                            Board.indexedMap
                                (\( i, j ) cell ->
                                    if ( x, y ) == ( i, j ) then
                                        (Just currentPlayer)
                                    else
                                        cell
                                )
                                board
                        , currentPlayer = nextPlayer
                        }

                    TogglePlayer ->
                        { board = board
                        , currentPlayer = nextPlayer
                        }

-- VIEW


svgView : Model -> Svg Msg
svgView =
    TicTacToeBase.svgView identity svgViewCell


svgViewCell : Board.Coords -> Cell.Cell -> Svg Msg
svgViewCell ( i, j ) model =
    Cell.svgView model (PlaceMark ( i, j ))
        |> SvgUtils.scale ((toFloat TicTacToeBase.cellSize) / 100.0)
        |> SvgUtils.translate ((T3.toInt i) * TicTacToeBase.cellSize) ((T3.toInt j) * TicTacToeBase.cellSize)
