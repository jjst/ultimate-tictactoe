module AI exposing (..)

import Board
import Cell
import Debug
import Player exposing (..)
import TicTacToe
import Tuple3
import UltimateTicTacToe exposing (GameState, Move, performMove)


type Difficulty
    = Easy
    | Normal
    | Hard



-- MODEL


nextMove : Difficulty -> GameState -> Maybe Move
nextMove difficulty gameState =
    let
        minimaxScore : Move -> Int
        minimaxScore move =
            minimax difficulty (performMove gameState.currentPlayer move gameState) 1 Minimize

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


minimax : Difficulty -> GameState -> Int -> Action -> Int
minimax difficulty ticTacToe depth action =
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
                        evalPosition difficulty ticTacToe
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
                                            minimax difficulty (performMove ticTacToe.currentPlayer move ticTacToe) (depth - 1) Minimize
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
                                            minimax difficulty (performMove ticTacToe.currentPlayer move ticTacToe) (depth - 1) Maximize
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


evalPosition : Difficulty -> GameState -> Scores
evalPosition difficulty gameState =
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
            rows |> List.map (scoreRow difficulty player) |> List.sum

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


scoreRow : Difficulty -> Player -> List TicTacToe.TicTacToeBoard -> Int
scoreRow difficulty player row =
    row |> List.map (score difficulty player) |> List.product



-- Give a winnability score to a board for a given player


score : Difficulty -> Player -> TicTacToe.TicTacToeBoard -> Int
score difficulty player ticTacToe =
    let
        coeffs =
            coefficients difficulty
    in
    case TicTacToe.winner ticTacToe of
        Just (Left p) ->
            if p == player then
                coeffs.won

            else
                coeffs.noChanceOfWinning

        Just (Right Draw) ->
            coeffs.noChanceOfWinning

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
            -- 2 in a row, last one is free
            if List.any (\r -> count (Just player) r == 2 && List.member Nothing r) rows then
                coeffs.canPlaceThirdInRow
                -- 1 in a row where 2 others are free

            else if List.any (\r -> count Nothing r == 2 && List.member (Just player) r) rows then
                coeffs.canPlaceSecondInRow
                -- 3 in row are free somewhere on this board

            else if List.any (\r -> count Nothing r == 3) rows then
                coeffs.canPlaceFirstInRow
                -- no chance of winning this board

            else
                coeffs.noChanceOfWinning


type alias Coefficients =
    { won : Int
    , noChanceOfWinning : Int
    , canPlaceThirdInRow : Int
    , canPlaceSecondInRow : Int
    , canPlaceFirstInRow : Int
    }


coefficients : Difficulty -> Coefficients
coefficients difficulty =
    case difficulty of
        Easy ->
            { won = 2
            , noChanceOfWinning = 1
            , canPlaceThirdInRow = 1
            , canPlaceSecondInRow = 1
            , canPlaceFirstInRow = 1
            }

        Normal ->
            { won = 3
            , noChanceOfWinning = 0
            , canPlaceThirdInRow = 2
            , canPlaceSecondInRow = 1
            , canPlaceFirstInRow = 1
            }

        Hard ->
            { won = 6
            , noChanceOfWinning = 0
            , canPlaceThirdInRow = 4
            , canPlaceSecondInRow = 2
            , canPlaceFirstInRow = 1
            }
