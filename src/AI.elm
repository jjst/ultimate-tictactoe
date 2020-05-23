module AI exposing (..)

import Board
import Cell
import Debug
import Player exposing (..)
import TicTacToe
import Tuple3
import UltimateTicTacToe exposing (GameState, Move, performMove)



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
            potentialMoves
                |> List.map (\m -> ( m, minimaxScore m ))
                |> List.sortBy Tuple.second
                |> List.reverse
                |> Debug.log "available moves"
    in
    scoredMoves
        |> List.head
        |> Debug.log "next move"
        |> Maybe.map Tuple.first



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
                    board
                        |> Board.get coords
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
                let
                    scores =
                        evalPosition ticTacToe
                in
                scores.o - scores.x

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
                                List.maximum values |> Maybe.withDefault -maxValue
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
                                List.minimum values |> Maybe.withDefault maxValue
                        in
                        min


type alias Scores =
    { o : Int
    , x : Int
    }



-- Heuristic to evaluate the current board


evalPosition : GameState -> Scores
evalPosition gameState =
    let
        rows : List (List TicTacToe.TicTacToeBoard)
        rows =
            Board.allRows
                |> List.map
                    (\row ->
                        row |> Tuple3.toList |> List.map (\coords -> Board.get coords gameState.board)
                    )

        scoreWholeBoardFor : Player -> Int
        scoreWholeBoardFor player =
            rows |> List.map (scoreRow player) |> List.sum

        scores =
            { o = scoreWholeBoardFor O, x = scoreWholeBoardFor X }

        -- If the player can choose to play on any grid, that counts as a bonus (or malus if it's our opponent)
        scoresWithBonus =
            case gameState.currentBoardCoords of
                Nothing ->
                    case gameState.currentPlayer of
                        O ->
                            { scores | o = scores.o + 20 }

                        X ->
                            { scores | x = scores.x + 20 }

                _ ->
                    scores
    in
    scoresWithBonus



-- Give a winnability score to a row of boards for a given player


scoreRow : Player -> List TicTacToe.TicTacToeBoard -> Int
scoreRow player row =
    row |> List.map (score player) |> List.product



-- Give a winnability score to a board for a given player


score : Player -> TicTacToe.TicTacToeBoard -> Int
score player ticTacToe =
    case TicTacToe.winner ticTacToe of
        Just (Left p) ->
            if p == player then
                6

            else
                0

        Just (Right Draw) ->
            0

        Nothing ->
            let
                rows : List (List Cell.Cell)
                rows =
                    Board.allRows
                        |> List.map
                            (\row ->
                                row |> Tuple3.toList |> List.map (\coords -> Board.get coords ticTacToe)
                            )

                count : Maybe Player -> List (Maybe Player) -> Int
                count item row =
                    row |> List.filter (\e -> e == item) |> List.length
            in
            if List.any (\r -> count (Just player) r == 2 && List.member Nothing r) rows then
                4

            else if List.any (\r -> count Nothing r == 2 && List.member (Just player) r) rows then
                2

            else if List.any (\r -> count Nothing r == 3) rows then
                1

            else
                0
