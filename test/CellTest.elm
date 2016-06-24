module Main exposing (..)

import ElmTest exposing (..)

import Cell
import Player


tests : Test
tests =
  suite
    "Cell"
    [ test "initializing cell" (assertEqual { mark = Nothing, currentPlayer = Player.X } Cell.init)
    ]


main : Program Never
main =
  runSuite tests
