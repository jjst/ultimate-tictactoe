module AITest exposing (..)

import ElmTest exposing (runSuite)
import ElmTestBDDStyle exposing (..)

import UltimateTicTacToe exposing (..)
import TicTacToe as T
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
    , let
          currentBoard = fromString Player.O (Just (I2,I2))
            """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            """ |> orCrash
      in
          it "evalPosition gives 0 value if nothing played"
            <| expect
                 (AI.evalPosition currentBoard)
               toBe
                 (0)
    , let
          currentBoard = fromString Player.O (Nothing)
            """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            """ |> orCrash
      in
          it "evalPosition gives 20 bonus if free to play anywhere"
            <| expect
                 (AI.evalPosition currentBoard)
               toBe
                 (20)
    , let
          currentBoard = fromString Player.X (Nothing)
            """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            """ |> orCrash
      in
          it "evalPosition gives -20 malus if opponent free to play anywhere"
            <| expect
                 (AI.evalPosition currentBoard)
               toBe
                 (-20)
    , let
          board = T.fromString Player.X
            """
            o o _
            _ _ _
            _ _ _
            """ |> orCrash
      in
          it "gives a score of 3 to a tictactoe with 2 in a row"
            <| expect
                 (AI.score Player.O board)
               toBe <|
                 (3)
    , let
          board = T.fromString Player.X
            """
            o o o
            _ _ _
            _ _ _
            """ |> orCrash
      in
          it "gives a score of 4 to a won tictactoe"
            <| expect
                 (AI.score Player.O board)
               toBe <|
                 (4)
    , let
          board = T.fromString Player.X
            """
            o o o
            _ _ _
            _ _ _
            """ |> orCrash
      in
          it "gives a score of 0 to a lost tictactoe"
            <| expect
                 (AI.score Player.X board)
               toBe <|
                 (0)
    , let
          board = T.fromString Player.X
            """
            o x o
            x o x
            o o x
            """ |> orCrash
      in
          it "gives a score of 0 to a draw tictactoe"
            <| expect
                 (AI.score Player.X board)
               toBe <|
                 (0)
    , let
          board = T.fromString Player.X
            """
            _ x _
            x o o
            x o x
            """ |> orCrash
      in
          it "gives a score of 0 to an unwinnable tictactoe"
            <| expect
                 (AI.score Player.O board)
               toBe <|
                 (0)
    , let
          board = T.fromString Player.X
            """
            _ x _
            x o o
            x o x
            """ |> orCrash
      in
          it "gives a score of 0 to an unwinnable tictactoe"
            <| expect
                 (AI.score Player.O board)
               toBe <|
                 (0)
    , let
          board = T.fromString Player.X
            """
            _ x _
            _ _ _
            o _ _
            """ |> orCrash
      in
          it "gives a score of 2 to one in a row"
            <| expect
                 (AI.score Player.O board)
               toBe <|
                 (2)
    , let
          board = T.fromString Player.X
            """
            _ x _
            _ _ _
            _ o x
            """ |> orCrash
      in
          it "gives a score of 1 to none in a row"
            <| expect
                 (AI.score Player.O board)
               toBe <|
                 (1)
    ]



main : Program Never
main =
  runSuite tests
