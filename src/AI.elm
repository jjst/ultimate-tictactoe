module AI exposing (..)

import UltimateTicTacToe
import TicTacToe
import Board
import Cell

type alias Move =
    { boardCoords: Board.Coords
    , cellCoords: Board.Coords
    }

toMessage : Move -> UltimateTicTacToe.Msg
toMessage { boardCoords, cellCoords } =
    UltimateTicTacToe.MetaPlaceMark boardCoords (TicTacToe.PlaceMark cellCoords Cell.PlaceMark)

-- List all valid moves given current state of the game
validMoves : UltimateTicTacToe.Model -> List Move
validMoves { board, currentPlayer, currentBoardCoords } =
    case UltimateTicTacToe.winner board of
        Nothing ->
            case currentBoardCoords of
                Nothing ->
                    let
                        moves : List Move
                        moves =
                            board |> Board.indexedMap (\coords b ->
                                    b |> validMovesOnBoard |> List.map (\c -> { boardCoords = coords, cellCoords = c })
                                     )
                                  |> Board.flatten
                                  |> List.concat
                    in
                       moves
                Just coords ->
                    coords
                        |> Board.get board
                        |> validMovesOnBoard
                        |> List.map (\c -> { boardCoords = coords, cellCoords = c })
        _ -> []

validMovesOnBoard : TicTacToe.Model -> List Board.Coords
validMovesOnBoard ticTacToe =
    case TicTacToe.winner ticTacToe.board of
        Nothing ->
            ticTacToe.board
                |> Board.indexedMap (\coords cell -> if cell.mark == Nothing then Just coords else Nothing)
                |> Board.flatten
                |> List.filterMap identity
        _ -> []
