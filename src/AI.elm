module AI exposing (..)

import Tuple3
import UltimateTicTacToe exposing (GameState, Move, performMove)
import TicTacToe
import Board
import Player exposing (..)
import Debug


-- MODEL


nextMove : GameState -> Maybe Move
nextMove gameState =
    let
        minimaxScore : Move -> Int
        minimaxScore move =
            minimax (performMove gameState.currentPlayer move gameState) 1 Minimize

        potentialMoves =
            validMoves gameState

        scoredMoves =
            potentialMoves |> List.map (\m -> ( m, minimaxScore m )) |> Debug.log "available moves"

        nextMove =
            scoredMoves
                |> List.sortBy Tuple.second
                |> List.reverse
                |> List.head
                |> Debug.log "next move"
                |> Maybe.map Tuple.first
    in
        nextMove



-- List all valid moves given current state of the game


validMoves : GameState -> List Move
validMoves { board, currentPlayer, currentBoardCoords } =
    case UltimateTicTacToe.winner board of
        Nothing ->
            case currentBoardCoords of
                Nothing ->
                    let
                        moves : List Move
                        moves =
                            board
                                |> Board.indexedMap
                                    (\coords b ->
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

        _ ->
            []


validMovesOnBoard : TicTacToe.TicTacToeBoard -> List TicTacToe.Move
validMovesOnBoard ticTacToeBoard =
    case TicTacToe.winner ticTacToeBoard of
        Nothing ->
            ticTacToeBoard
                |> Board.indexedMap
                    (\coords cell ->
                        if cell == Nothing then
                            Just coords
                        else
                            Nothing
                    )
                |> Board.flatten
                |> List.filterMap identity

        _ ->
            []


maxValue =
    100000


type Action
    = Maximize
    | Minimize



-- Assume that AI is always O for now


minimax : GameState -> Int -> Action -> Int
minimax ticTacToe depth action =
    case UltimateTicTacToe.winner ticTacToe.board of
        Just winner ->
            case winner of
                Left X ->
                    -maxValue

                Left O ->
                    maxValue

                Right Draw ->
                    0

        Nothing ->
            if depth == 0 then
                evalPosition ticTacToe
            else
                case action of
                    Maximize ->
                        let
                            values =
                                validMoves ticTacToe
                                    |> List.map
                                        (\move ->
                                            minimax (performMove ticTacToe.currentPlayer move ticTacToe) (depth - 1) Minimize
                                        )

                            max =
                                List.maximum values |> Maybe.withDefault (-maxValue)
                        in
                            max

                    Minimize ->
                        let
                            values =
                                validMoves ticTacToe
                                    |> List.map
                                        (\move ->
                                            minimax (performMove ticTacToe.currentPlayer move ticTacToe) (depth - 1) Maximize
                                        )

                            min =
                                List.minimum values |> Maybe.withDefault (maxValue)
                        in
                            min



-- Heuristic to evaluate the current board


evalPosition : GameState -> Int
evalPosition model =
    let
        wonBoardsDelta =
            (wonBoards model O - wonBoards model X)

        winnableBoardsDelta =
            (winnableBoards model O - winnableBoards model X)

        -- If the player can choose to play on any grid, that counts as a bonus (or malus if it's our opponent)
        unrestrictedMovementBonus =
            case model.currentBoardCoords of
                Nothing ->
                    case model.currentPlayer of
                        O ->
                            70

                        X ->
                            -70

                _ ->
                    0
    in
        wonBoardsDelta * 100 + winnableBoardsDelta * 35 + unrestrictedMovementBonus


winnableBoards : GameState -> Player -> Int
winnableBoards model player =
    model.board
        |> Board.flatten
        |> List.filter (\b -> isWinnable b player)
        |> List.length



-- A board is winnable if it has 2 in a row


isWinnable : TicTacToe.TicTacToeBoard -> Player -> Bool
isWinnable ticTacToeBoard player =
    let
        isRowWinnable : List (Maybe Player) -> Bool
        isRowWinnable row =
            let
                markCount =
                    row |> List.filter (\e -> e == Just player) |> List.length
            in
                markCount == 2 && List.member Nothing row

        rows : List (List (Maybe Player))
        rows =
            Board.allRows
                |> List.map (\row -> row |> Tuple3.toList |> List.map (\coords -> (Board.get ticTacToeBoard coords)))
    in
        List.any isRowWinnable rows


wonBoards : GameState -> Player -> Int
wonBoards model player =
    let
        subBoards =
            Board.flatten model.board

        wonBoards =
            subBoards |> List.filter (\b -> TicTacToe.winner b == Just (Left player)) |> List.length
    in
        wonBoards



