module CellTest exposing (tests)

import Test exposing (..)
import Expect exposing (..)
import ElmTestBDDStyle exposing (..)
import Cell
import Player


tests : Test
tests =
    describe "A Cell"
        [ it "builds a X from a string" <|
            expect
                (Cell.fromString "X")
                to equal
                (Ok (Just Player.X))
        , it "builds a O from a string" <|
            expect
                (Cell.fromString "O")
                to equal
                (Ok (Just Player.O))
        , it "builds an empty cell from a string" <|
            expect
                (Cell.fromString "_")
                to equal
                (Ok Nothing)
        , it "errors out when passed an invalid string" <|
            expect
                (Cell.fromString "%")
                to equal
                (Err "Invalid character: %")
        ]
