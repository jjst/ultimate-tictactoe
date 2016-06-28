module UltimateTicTacToeTest exposing (..)

import ElmTest exposing (runSuite)
import ElmTestBDDStyle exposing (..)

import Debug

import UltimateTicTacToe exposing (..)
import Board
import Tuple3 exposing (..)
import TicTacToe
import Cell as C
import Player

initialBoard = init

tests : Test
tests =
  describe "An updated UltimateTicTacToeBoard Model"
    [ it "toggles the current player"
        <| expect
            (update (MetaPlaceMark (I1,I1) (TicTacToe.PlaceMark (I1,I1) C.PlaceMark)) initialBoard).currentPlayer
           toBe Player.O
    , let
          str =
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
            """
      in
          it "should construct a default board model"
            <| expect
                 (fromString Player.X Nothing str)
               toBe
                 (Ok initialBoard)
    , let
          str =
            """
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | x _ _
             _ _ _ | _ _ _ | _ _ o
            -------+-------+-------
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
             _ _ _ | _ _ _ | _ _ _
            """
          subBoardResult = TicTacToe.fromString Player.X
            """
             _ _ _
             x _ _
             _ _ o
            """
          subBoard = case subBoardResult of
            Ok b -> b
            Err msg -> Debug.crash msg
      in
          it "should contain the expected subboard"
            <| expect
                 (fromString Player.X Nothing str)
               toBe
                 (Ok { initialBoard | board = Board.indexedMap (\coords s ->
                       if coords == (I2, I3) then subBoard else s) initialBoard.board
                 })
    ]


main : Program Never
main =
  runSuite tests
