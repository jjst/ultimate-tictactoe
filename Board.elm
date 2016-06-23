module Board exposing (Board, Coords, Row, flatten, indexedMap, allRows, winningRow, cellAt, winner)

import Cell
import Player exposing (..)
import Tuple3 exposing (..)
import Tuple3 as T3

import List as L
import Maybe exposing (withDefault, andThen)


type alias Board = Tuple3 (Tuple3 Cell.Model)

type alias Coords = (T3.Index, T3.Index)

type alias Row = Tuple3 Coords

indexedMap : (Coords -> a -> b) -> Tuple3 (Tuple3 a) -> Tuple3 (Tuple3 b)
indexedMap f mat3 =
    mat3 |> T3.indexedMap (\i row -> row |> T3.indexedMap (\j val -> f (i,j) val))

allRows : List Row
allRows =
    let
        range = T3.range
        horizontals = T3.map (\y -> T3.map (\x -> (x,y)) range) range |> T3.toList
        verticals = T3.map (\y -> T3.map (\x -> (y,x)) range) range |> T3.toList
        diagonals =
            [ ( (I1,I1), (I2,I2), (I3,I3) )
            , ( (I1,I3), (I2,I2), (I3,I1) )
            ]
    in
        horizontals  ++ verticals ++ diagonals

flatten : Tuple3 (Tuple3 a) -> List a
flatten board =
  board
    |> T3.map T3.toList
    |> T3.toList
    |> L.concat


winningRow : Board -> Maybe Row
winningRow board =
  allRows
    |> L.filter (\r -> isWinningRow board r)
    |> L.head

isWinningRow : Board -> Row -> Bool
isWinningRow board row =
  let
    marks =
        row
        |> T3.map (\coords -> (cellAt board coords).mark)
        |> toList
  in
     L.all (\m -> m == Just X) marks || L.all (\m -> m == Just O) marks

get : Int -> List a -> Maybe a
get idx list =
    list |> L.drop idx |> L.head

cellAt : Board -> Coords -> Cell.Model
cellAt board (x, y) =
    (board !! x) !! y

-- Returns who the winner is (can be a draw),
-- or Nothing if the game is still in progress.

winner : Board -> Maybe Winner
winner board =
    case winningRow board of
      Just (first, _, _) -> Maybe.map Left (cellAt board first).mark
      _ ->
        let
          -- flatten board and !check for any unmarked
          hasEmptyCells = board
            |> flatten
            |> L.any (\cell -> cell.mark == Nothing)
        in
          if hasEmptyCells then Nothing else Just (Right Draw)
