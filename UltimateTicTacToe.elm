import TicTacToe
import Player exposing (..)
import Board exposing (..)
import SvgUtils
import TicTacToeBase
import TicTacToeBase exposing (strikeThrough, cellSize, boardSize, grid)

import List as L
import Tuple3 as T3
import Html.Attributes
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias UltimateTicTacToeBoard = Board TicTacToe.Model

main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

-- MODEL

type alias Model =
    { board : UltimateTicTacToeBoard
    , currentPlayer : Player
    , currentBoardCoords : Board.Coords
    }

init : Model
init =
    { board = T3.fill (T3.fill TicTacToe.init)
    , currentPlayer = X
    , currentBoardCoords = (T3.I2, T3.I2) -- FIXME!!!!
    }

boardOwner : TicTacToe.Model -> Maybe Player
boardOwner boardModel =
    case TicTacToe.winner boardModel.board of
        Nothing -> Nothing
        Just winner -> case winner of
            Left player -> Just player
            Right Draw -> Nothing

winner : UltimateTicTacToeBoard -> Maybe Winner
winner = Board.winner boardOwner


-- UPDATE

type Msg = MetaPlaceMark Coords TicTacToe.Msg

update : Msg -> Model -> Model
update msg ({board, currentPlayer, currentBoardCoords} as model) =
    let
        nextPlayer = opponent currentPlayer
    in
       case winner board of
           Just _ -> model
           Nothing ->
            case msg of
                MetaPlaceMark (x,y) ((TicTacToe.PlaceMark cellCoords cellMsg) as msg) ->
                    if (x,y) == currentBoardCoords then
                        { board = indexedMap (\(i,j) subBoard ->
                            if (x,y) == (i,j) then TicTacToe.update msg subBoard else TicTacToe.update TicTacToe.TogglePlayer subBoard) board
                        , currentPlayer = nextPlayer
                        , currentBoardCoords = cellCoords
                        }
                    else
                        model
                _ -> model


-- VIEW

view : Model -> Html Msg
view model =
    let
        size = (toString TicTacToeBase.boardSize)
        divStyle =
          Html.Attributes.style
            [ ("margin", "auto")
            , ("width", size ++ "px")
            ]
    in
        div [ divStyle ]
            [
            svg [ viewBox ("0 0 " ++ size ++ " " ++ size), width (size ++ "px") ]
                [(svgView model)]
            ]


svgView : Model -> Svg Msg
svgView model =
    let
        board = model.board
        vb = svgViewBoard model.currentBoardCoords
        cells = g [] (flatten <| (indexedMap vb board))
        st = case (winningRow boardOwner board) of
            Just (first,middle,last) -> [ strikeThrough cellSize first last ]
            _ -> []
    in
          g [] ([ cells, (grid cellSize) ] ++ st)



svgViewBoard : Coords -> Coords -> TicTacToe.Model -> Svg Msg
svgViewBoard currentBoardCoords ((i,j) as coords) ({board, currentPlayer} as model) =
    let
        node = TicTacToe.svgView model
        nodeWithOpacity = if coords /= currentBoardCoords then g [ opacity "0.25", fill "red" ] [ node ] else node
    in
       nodeWithOpacity
          |> SvgUtils.scale (1.0/3.0)
          |> SvgUtils.translate ((T3.toInt i)*TicTacToeBase.cellSize) ((T3.toInt j)*TicTacToeBase.cellSize)
          |> App.map (MetaPlaceMark coords)
