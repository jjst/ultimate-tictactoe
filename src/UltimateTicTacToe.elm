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
    , currentBoardCoords : Maybe Board.Coords
    }

init : Model
init =
    { board = T3.fill (T3.fill TicTacToe.init)
    , currentPlayer = X
    , currentBoardCoords = Nothing
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
                    case currentBoardCoords of
                        Nothing ->
                            { board = indexedMap (\(i,j) subBoard ->
                                if (x,y) == (i,j) then TicTacToe.update msg subBoard else TicTacToe.update TicTacToe.TogglePlayer subBoard) board
                            , currentPlayer = nextPlayer
                            , currentBoardCoords = if (TicTacToe.winner (get board cellCoords).board == Nothing) then Just cellCoords else Nothing
                            }
                        Just c ->
                            if (x,y) == c then
                                { board = indexedMap (\(i,j) subBoard ->
                                    if (x,y) == (i,j) then TicTacToe.update msg subBoard else TicTacToe.update TicTacToe.TogglePlayer subBoard) board
                                , currentPlayer = nextPlayer
                                , currentBoardCoords = if (TicTacToe.winner (get board cellCoords).board == Nothing) then Just cellCoords else Nothing
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



svgViewBoard : (Maybe Coords) -> Coords -> TicTacToe.Model -> Svg Msg
svgViewBoard currentBoardCoords ((i,j) as coords) ({board, currentPlayer} as model) =
    let
        node = TicTacToe.svgView model
        s = (toString TicTacToeBase.boardSize)
        group = case currentBoardCoords of
            Just cds ->
                if (coords /= cds) then
                   g [ opacity "0.20" ] [ node ]
                else
                   g [] [ rect [ x "0", y "0", fill "cyan", width s, height s, fillOpacity "0.05" ] [], node ]
            Nothing -> node
    in
       group
          |> SvgUtils.scale (1.0/3.0)
          |> SvgUtils.translate ((T3.toInt i)*TicTacToeBase.cellSize) ((T3.toInt j)*TicTacToeBase.cellSize)
          |> App.map (MetaPlaceMark coords)
