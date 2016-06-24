module Board exposing (Board, Coords, Row, OwnerFunction, flatten, indexedMap, allRows, winningRow, get, winner)

import Player exposing (..)
import Tuple3 exposing (..)
import Tuple3 as T3

import List as L
import Maybe exposing (withDefault, andThen)


type alias Board a = Tuple3 (Tuple3 a)

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

get : Board a -> Coords -> a
get board (x, y) =
    (board !! x) !! y


type alias OwnerFunction a = (a -> Maybe Player)

winningRow : OwnerFunction a -> Board a -> Maybe Row
winningRow owner board =
  allRows
    |> L.filter (\r -> isWinningRow owner board r)
    |> L.head

isWinningRow : OwnerFunction a -> Board a -> Row -> Bool
isWinningRow owner board row =
  let
    owners =
        row
        |> T3.map (\coords -> owner (get board coords))
        |> toList
  in
     L.all (\m -> m == Just X) owners || L.all (\m -> m == Just O) owners

-- Returns who the winner is (can be a draw),
-- or Nothing if the game is still in progress.

winner : OwnerFunction a -> Board a -> Maybe Winner
winner owner board =
    case winningRow owner board of
      Just (first, _, _) -> Maybe.map Left (owner (get board first))
      _ ->
        let
          -- flatten board and !check for any unmarked
          hasEmptyCells = board
            |> flatten
            |> L.any (\cell -> owner cell == Nothing)
        in
          if hasEmptyCells then Nothing else Just (Right Draw)
