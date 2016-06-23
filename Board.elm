module Board exposing (Board, Row, mapWithIndex, allRows, winningRow, cellAt, winner)

import Cell
import Player exposing (..)

import List as L
import Maybe exposing (withDefault, andThen)


type alias Board = List (List Cell.Model)

type alias Row = List (Int, Int)

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
      Just (first::_) -> Maybe.map Left (cellAt board first).mark
      _ ->
        let
          -- flatten board and !check for any unmarked
          hasEmptyCells = board
            |> L.concat
            |> L.any (\cell -> cell.mark == Nothing)
        in
          if hasEmptyCells then Nothing else Just (Right Draw)
