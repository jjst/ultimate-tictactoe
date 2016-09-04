module AITest exposing (..)

import ElmTest exposing (runSuite)
import ElmTestBDDStyle exposing (..)

import AI
import TicTacToe
import Player
import Tuple3 exposing (..)

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
    ]



main : Program Never
main =
  runSuite tests
