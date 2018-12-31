module Tests exposing (..)

import Test exposing (..)
import Expect exposing (..)

import CellTest
import Tuple3Test
import UltimateTicTacToeTest
import AITest
import TicTacToeTest

all : Test
all =
    describe "elmtimate-tictactoe"
        [ CellTest.tests
        , Tuple3Test.tests
        , UltimateTicTacToeTest.tests
        , AITest.tests
        , TicTacToeTest.tests
        ]
