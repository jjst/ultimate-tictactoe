module CellTest exposing (..)

import ElmTest exposing (runSuite)
import ElmTestBDDStyle exposing (..)

import Cell
import Player

tests : Test
tests =
  describe "A Cell"
    [ it "initialises an empty cell"
        <| expect Cell.init toBe { mark = Nothing, currentPlayer = Player.X }
    , it "builds a X from a string"
        <| expect
             (Cell.fromString Player.X "X")
           toBe
             (Ok { mark = Just Player.X, currentPlayer = Player.X })
    , it "builds a O from a string"
        <| expect
             (Cell.fromString Player.X "O")
           toBe
             (Ok { mark = Just Player.O, currentPlayer = Player.X })
    , it "builds an empty cell from a string"
        <| expect
             (Cell.fromString Player.X "_")
           toBe
             (Ok { mark = Nothing, currentPlayer = Player.X })
    , it "errors out when passed an invalid string"
        <| expect
             (Cell.fromString Player.X "%")
           toBe
             (Err "Invalid character: %" )
    ]



main : Program Never
main =
  runSuite tests
