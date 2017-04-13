module CellTest exposing (tests)

import Test exposing (..)
import Expect exposing (..)
import ElmTestBDDStyle exposing (..)
import Cell
import Player


tests : Test
tests =
    describe "A Cell"
        [ it "initialises an empty cell" <|
            expect Cell.init to equal { mark = Nothing, currentPlayer = Player.X }
        , it "builds a X from a string" <|
            expect
                (Cell.fromString Player.X "X")
                to equal
                (Ok { mark = Just Player.X, currentPlayer = Player.X })
        , it "builds a O from a string" <|
            expect
                (Cell.fromString Player.X "O")
                to equal
                (Ok { mark = Just Player.O, currentPlayer = Player.X })
        , it "builds an empty cell from a string" <|
            expect
                (Cell.fromString Player.X "_")
                to equal
                (Ok { mark = Nothing, currentPlayer = Player.X })
        , it "errors out when passed an invalid string" <|
            expect
                (Cell.fromString Player.X "%")
                to equal
                (Err "Invalid character: %")
        ]
