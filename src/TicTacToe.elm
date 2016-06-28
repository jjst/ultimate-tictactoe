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
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias TicTacToeBoard = Board Cell.Model

main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

-- MODEL

type alias Model = TicTacToeBase.Model Cell.Model

init : Model
init = TicTacToeBase.init Cell.init

cellOwner : Cell.Model -> Maybe Player
cellOwner cell = cell.mark

winner : TicTacToeBoard -> Maybe Winner
winner = Board.winner cellOwner

fromString : Player -> String -> Result String Model
fromString player str =
    let
        liftResult : List (Result a b) -> Result a (List b)
        liftResult list =
            list |> L.foldr (\result listResult -> listResult `Result.andThen` (\list -> Result.map (flip (::) list) result)) (Ok [])
        parseLine : String -> Result String (List Cell.Model)
        parseLine l = l
          |> String.trim
          |> String.split " "
          |> L.map (Cell.fromString player)
          |> liftResult
        parsed : Result String (List (List (Cell.Model)))
        parsed = str
          |> String.trim
          |> String.lines
          |> L.map parseLine
          |> liftResult
        boardResult =
           Result.map (
               L.map (
                   T3.fromList >> Result.fromMaybe "Wrong number of items in row"
               )
           ) parsed
           `Result.andThen` liftResult
           `Result.andThen` (
               T3.fromList >> Result.fromMaybe "Wrong number of rows"
           )
    in boardResult |> Result.map (\b -> { board = b, currentPlayer = player })

-- UPDATE

type Msg
    = PlaceMark Board.Coords Cell.Msg
    | TogglePlayer

update : Msg -> Model -> Model
update msg ({board, currentPlayer} as model) =
    let
        nextPlayer = opponent currentPlayer
    in
       case winner board of
           Just _ -> model
           Nothing ->
            case msg of
                PlaceMark (x, y) msg ->
                    { board = Board.indexedMap (\(i,j) cell ->
                        if (x,y) == (i,j) then Cell.update msg cell else { cell | currentPlayer = nextPlayer }) board
                    , currentPlayer = nextPlayer
                    }
                TogglePlayer ->
                    { board = Board.indexedMap (\(i,j) cell -> { cell | currentPlayer = nextPlayer }) board
                    , currentPlayer = nextPlayer
                    }


-- VIEW

svgView : Model -> Svg Msg
svgView = TicTacToeBase.svgView cellOwner svgViewCell

view : Model -> Html Msg
view = TicTacToeBase.view cellOwner svgViewCell


svgViewCell : Board.Coords -> Cell.Model -> Svg Msg
svgViewCell (i,j) model =
    Cell.svgView model
      |> SvgUtils.scale ((toFloat TicTacToeBase.cellSize)/100.0)
      |> SvgUtils.translate ((T3.toInt i)*TicTacToeBase.cellSize) ((T3.toInt j)*TicTacToeBase.cellSize)
      |> App.map (PlaceMark (i,j))
