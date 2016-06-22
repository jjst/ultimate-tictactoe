import Cell
import Player exposing (..)
import SvgUtils

import Maybe exposing (withDefault, andThen)
import List as L
import Html exposing (Html, button, div, text)
import Html.App as App
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)



main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }

-- MODEL

type alias Board = List (List Cell.Model)

type alias Row = List (Int, Int)

type alias Model =
    { board : Board
    , currentPlayer : Player
    }

type Winner = Cross | Circle | Draw

fromPlayer : Player -> Winner
fromPlayer player = case player of
  O -> Circle
  X -> Cross


init : Model
init =
    { board = List.repeat 3 (List.repeat 3 Cell.init)
    , currentPlayer = X
    }

mapWithIndex : ((Int,Int) -> a -> b) -> List (List a) -> List (List b)
mapWithIndex f matrix =
    L.indexedMap (\i row -> L.indexedMap (\j val -> f (i,j) val) row) matrix

allRows : List Row
allRows =
    let
        range = [0..2]
        horizontals = List.map (\y -> List.map (\x -> (x,y)) range) range
        verticals = List.map (\y -> List.map (\x -> (y,x)) range) range
        diagonals = [[(0,0), (1,1), (2,2)], [(0,2), (1,1), (2,0)]]
    in
        horizontals  ++ verticals ++ diagonals


winningRow : Board -> Maybe Row
winningRow board =
  allRows
    |> L.filter (\r -> isWinningRow board r)
    |> L.head

isWinningRow : Board -> Row -> Bool
isWinningRow board row =
  let
    marks = L.map (\coords -> (cellAt board coords).mark) row
  in
     L.all (\m -> m == Just X) marks || L.all (\m -> m == Just O) marks

get : Int -> List a -> Maybe a
get idx list =
    list |> L.drop idx |> L.head

cellAt : Board -> (Int, Int) -> Cell.Model
cellAt board (x, y) =
    let
        cell = (get x board) `andThen` (get y)
    in
       withDefault (Cell.init) cell -- FIXME don't like having to use withDefault here


-- Returns who the winner is (can be a draw),
-- or Nothing if the game is still in progress.

winner : Board -> Maybe Winner
winner board =
    case winningRow board of
      Just (first::_) -> Maybe.map fromPlayer (cellAt board first).mark
      _ ->
        let
          -- flatten board and !check for any unmarked
          hasEmptyCells = board
            |> L.concat
            |> L.any (\cell -> cell.mark == Nothing)
        in
          if hasEmptyCells then Nothing else Just Draw


-- UPDATE

type Msg = PlaceMark Int Int Cell.Msg

update : Msg -> Model -> Model
update msg ({board, currentPlayer} as model) =
    let
        nextPlayer = opponent currentPlayer
    in
       case winner board of
           Just _ -> model
           Nothing ->
            case msg of
                PlaceMark x y msg ->
                    { board = mapWithIndex (\(i,j) cell ->
                        if (x,y) == (i,j) then Cell.update msg cell else { cell | currentPlayer = nextPlayer }) board
                    , currentPlayer = nextPlayer
                    }


-- VIEW

view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 800 800", width "800px" ]
        [(svgView model)]

svgView : Model -> Svg Msg
svgView {board, currentPlayer} =
    let
        cells = g [] (L.concat <| mapWithIndex svgViewCell board)
        st = case (winningRow board) of
            Just [first,middle,last] -> [ strikeThrough first last ]
            _ -> []
    in
          g [] ([ cells, grid ] ++ st)

grid : Svg a
grid =
    g [ gridStyle ]
      [ line [ x1 "100", y1 "5", x2 "100", y2 "295" ] []
      , line [ x1 "200", y1 "5", x2 "200", y2 "295" ] []
      , line [ x1 "5", y1 "100", x2 "295", y2 "100" ] []
      , line [ x1 "5", y1 "200", x2 "295", y2 "200" ] []
      ]

strikeThrough: (Int,Int) -> (Int,Int) -> Svg a
strikeThrough (i1,j1) (i2,j2) =
    let
        toSvgCoords = \x offset -> (toFloat x + 0.5 + offset) * 100 |> toString
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


svgViewCell : (Int, Int) -> Cell.Model -> Svg Msg
svgViewCell (i,j) model =
    App.map (PlaceMark i j) (SvgUtils.translate (i*100) (j*100) (Cell.svgView model))
