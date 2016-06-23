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


type alias TicTacToeBoard = Board Cell.Model

main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

-- MODEL

type alias Model =
    { board : TicTacToeBoard
    , currentPlayer : Player
    }

init : Model
init =
    { board = T3.fill (T3.fill Cell.init)
    , currentPlayer = X
    }

cellOwner : Cell.Model -> Maybe Player
cellOwner cell = cell.mark


-- UPDATE

type Msg = PlaceMark T3.Index T3.Index Cell.Msg

update : Msg -> Model -> Model
update msg ({board, currentPlayer} as model) =
    let
        nextPlayer = opponent currentPlayer
    in
       case winner cellOwner board of
           Just _ -> model
           Nothing ->
            case msg of
                PlaceMark x y msg ->
                    { board = indexedMap (\(i,j) cell ->
                        if (x,y) == (i,j) then Cell.update msg cell else { cell | currentPlayer = nextPlayer }) board
                    , currentPlayer = nextPlayer
                    }


-- VIEW

cellSize = 220

view : Model -> Html Msg
view model =
    let
        size = (toString (cellSize*3))
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
svgView {board, currentPlayer} =
    let
        cells = g [] (flatten <| (indexedMap svgViewCell board))
        st = case (winningRow cellOwner board) of
            Just (first,middle,last) -> [ strikeThrough first last ]
            _ -> []
    in
          g [] ([ cells, grid ] ++ st)

grid : Svg a
grid =
    let
        offset = cellSize / 20
    in
        g [ gridStyle ]
          [ line [ x1 (toString cellSize), y1 (toString offset), x2 (toString cellSize), y2 (toString (3*cellSize - offset)) ] []
          , line [ x1 (toString (2*cellSize)), y1 (toString offset), x2 (toString (2*cellSize)), y2 (toString (3*cellSize - offset)) ] []
          , line [ x1 (toString offset), y1 (toString cellSize), x2 (toString (3*cellSize - offset)), y2 (toString cellSize) ] []
          , line [ x1 (toString offset), y1 (toString (2*cellSize)), x2 (toString (3*cellSize - offset)), y2 (toString (2*cellSize)) ] []
          ]

strikeThrough: Coords -> Coords -> Svg a
strikeThrough (i1,j1) (i2,j2) =
    let
        toSvgCoords = \x offset -> (T3.toFloat x + 0.5 + offset) * cellSize |> toString
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


svgViewCell : Coords -> Cell.Model -> Svg Msg
svgViewCell (i,j) model =
    Cell.svgView model
      |> SvgUtils.scale ((toFloat cellSize)/100.0)
      |> SvgUtils.translate ((T3.toFloat i)*cellSize) ((T3.toFloat j)*cellSize)
      |> App.map (PlaceMark i j)
