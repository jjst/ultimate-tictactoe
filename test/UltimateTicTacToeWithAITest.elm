module AITest exposing (..)

import ElmTest exposing (runSuite)
import ElmTestBDDStyle exposing (..)

import UltimateTicTacToe exposing (..)
import UltimateTicTacToeWithAI as AI
import TicTacToe
import Player
import Tuple3 exposing (..)

orCrash : Result String a -> a
orCrash result = case result of
    Err s -> Debug.crash s
    Ok a -> a

tests : Test
tests =
  describe "validMovesOnBoard"
    [ let
          str =
            """
            o o o
            _ _ _
            _ _ _
            """
          board = case (TicTacToe.fromString Player.X str) of
            Ok b -> b
            _ -> Debug.crash "Invalid board"
      in
          it "returns no valid moves if board is won"
            <| expect
                 (AI.validMovesOnBoard board)
               toBe
                 ([])
    , let
          str =
            """
            o o x
            x _ _
            _ x x
            """
          board = case (TicTacToe.fromString Player.X str) of
            Ok b -> b
            _ -> Debug.crash "Invalid board"
      in
          it "returns valid moves"
            <| expect
                 (AI.validMovesOnBoard board)
               toBe
                 ([(I2,I2),(I2,I3),(I3,I1)])
    , let
          currentBoard = fromString Player.O (Just (I2,I2))
            """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            """ |> orCrash
      in
          it "has the correct number of valid moves"
            <| expect
                 (List.length (AI.validMoves currentBoard))
               toBe
                 (7)
    , let
          currentBoard = fromString Player.O (Just (I2,I2))
            """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            """ |> orCrash
      in
          it "plays the winning move"
            <| expect
                 (AI.nextMove currentBoard)
               toBe
                 (Just {boardCoords = (I2,I2), cellCoords = (I2,I3)})
    ]



main : Program Never
main =
  runSuite tests
