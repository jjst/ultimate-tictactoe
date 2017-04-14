module TicTacToeTest exposing (tests)

import Test exposing (..)
import Expect exposing (..)
import ElmTestBDDStyle exposing (..)
import TicTacToe exposing (..)
import Tuple3 exposing (..)
import TicTacToe
import Player
import Board


initialBoard =
    init


tests : Test
tests =
    describe "A tic-tac-toe model"
        [ let
            str =
                """
            _ _ _
            _ _ _
            _ _ _
            """
          in
            it "builds the default board from a string full of _'s" <|
                expect
                    (fromString Player.X str)
                    to equal
                    (Ok initialBoard)
        , let
            str =
                """
            o o o
            x x x
            _ _ _
            """
          in
            it "fills each cell according to their string symbol" <|
                expect
                    (fromString Player.X str)
                    to equal
                    (Ok
                        { initialBoard
                            | board =
                                Board.indexedMap
                                    (\( i, j ) cell ->
                                        case i of
                                            I1 ->
                                                Just Player.O

                                            I2 ->
                                                Just Player.X

                                            I3 ->
                                                Nothing
                                    )
                                    initialBoard.board
                        }
                    )
        , let
            str =
                """
            o o o
            o o o
            x x x
            _ _ _
            """
          in
            it "fails to parse if there are to many rows" <|
                expect
                    (fromString Player.X str)
                    to equal
                <|
                    (Err "Wrong number of rows")
        , let
            str =
                """
            o o o
            o o o _
            x x x
            """
          in
            it "fails to parse if there are to many cols" <|
                expect
                    (fromString Player.X str)
                    to equal
                <|
                    (Err "Wrong number of items in row")
        , let
            str =
                """
            o o o
            o j o
            x x x
            """
          in
            it "fails to parse invalid chars" <|
                expect
                    (fromString Player.X str)
                    to equal
                <|
                    (Err "Invalid character: j")
        ]
