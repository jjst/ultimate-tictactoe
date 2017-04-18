module UltimateTicTacToeWithAITest exposing (..)

import Test exposing (..)
import Expect exposing (..)
import ElmTestBDDStyle exposing (..)
import UltimateTicTacToe exposing (..)
import TicTacToe as T
import UltimateTicTacToeWithAI as AI
import TicTacToe
import Player
import Tuple3 exposing (..)


orCrash : Result String a -> a
orCrash result =
    case result of
        Err s ->
            Debug.crash s

        Ok a ->
            a


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

            board =
                case (TicTacToe.fromString str) of
                    Ok b ->
                        b

                    _ ->
                        Debug.crash "Invalid board"
          in
            it "returns no valid moves if board is won" <|
                expect
                    (AI.validMovesOnBoard board)
                    to equal
                    ([])
        , let
            str =
                """
            o o x
            x _ _
            _ x x
            """

            board =
                case (TicTacToe.fromString str) of
                    Ok b ->
                        b

                    _ ->
                        Debug.crash "Invalid board"
          in
            it "returns valid moves" <|
                expect
                    (AI.validMovesOnBoard board)
                    to equal
                    ([ ( I2, I2 ), ( I2, I3 ), ( I3, I1 ) ])
        , let
            currentBoard =
                fromString Player.O
                    (Just ( I2, I2 ))
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
            """
                    |> orCrash
          in
            it "has the correct number of valid moves" <|
                expect
                    (List.length (AI.validMoves currentBoard))
                    to equal
                    (7)
        , let
            currentBoard =
                fromString Player.O
                    (Just ( I2, I2 ))
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
            """
                    |> orCrash
          in
            it "plays the winning move" <|
                expect
                    (AI.nextMove currentBoard)
                    to equal
                    (Just { boardCoords = ( I2, I2 ), cellCoords = ( I2, I3 ) })
        , let
            currentBoard =
                fromString Player.O
                    (Just ( I2, I2 ))
                    """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            """
                    |> orCrash
          in
            it "evalPosition gives 100 bonus per won board" <|
                expect
                    (AI.evalPosition currentBoard)
                    to equal
                    (200)
        , let
            currentBoard =
                fromString Player.O
                    (Just ( I2, I2 ))
                    """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | x x x | _ _ _
             _ _ _ | _ _ _ | x x x
            """
                    |> orCrash
          in
            it "evalPosition gives -100 malus per lost board" <|
                expect
                    (AI.evalPosition currentBoard)
                    to equal
                    (-100)
        , let
            currentBoard =
                fromString Player.O
                    (Nothing)
                    """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | x x x
            """
                    |> orCrash
          in
            it "evalPosition gives 70 bonus if free to play anywhere" <|
                expect
                    (AI.evalPosition currentBoard)
                    to equal
                    (70)
        , let
            currentBoard =
                fromString Player.X
                    (Nothing)
                    """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | o o o | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | x x x
            """
                    |> orCrash
          in
            it "evalPosition gives -70 malus if opponent free to play anywhere" <|
                expect
                    (AI.evalPosition currentBoard)
                    to equal
                    (-70)
        , let
            board =
                T.fromString
                    """
            o o _
            _ _ _
            _ _ _
            """
                    |> orCrash
          in
            it "detects a tictactoe with 2 in a row as winnable" <|
                expect
                    (AI.isWinnable board Player.O)
                    to equal
                <|
                    (True)
        , let
            board =
                T.fromString
                    """
            o o x
            _ _ _
            _ _ _
            """
                    |> orCrash
          in
            it "detects a blocked tictactoe with 2 in a row as non-winnable" <|
                expect
                    (AI.isWinnable board Player.O)
                    to equal
                <|
                    (False)
        , let
            board =
                T.fromString
                    """
            o x _
            _ _ _
            _ _ x
            """
                    |> orCrash
          in
            it "detects a tictactoe without 2 in a row as non winnable" <|
                expect
                    (AI.isWinnable board Player.X)
                    to equal
                <|
                    (False)
        ]
