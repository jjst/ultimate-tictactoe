module TicTacToe exposing (..)

import Cell
import Player exposing (..)
import Board exposing (Board)
import Board
import SvgUtils
import TicTacToeBase

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
