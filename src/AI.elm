module AI exposing (..)

import Board
import Cell
import Debug
import Player exposing (..)
import Random
import Random.Extra
import TicTacToe
import Tuple3
import UltimateTicTacToe exposing (GameState, Move, performMove)


type Difficulty
    = Easy
    | Normal
    | Hard

-- A scored move - the higher the score the most likely it is to win us the game
type alias ScoredMove =
    { move : Move
    , score : Int
    }

-- MODEL


nextMove : (Maybe Move -> msg) -> Difficulty -> GameState -> Cmd msg
nextMove f difficulty gameState =
    let
        randomNextMove =
            gameState
                |> scoreMoves -- score each available move
                |> degradeMoves difficulty -- degrade the moves according to difficulty level
                |> Random.andThen pickMove -- pick one of the best scored moves
    in
    Random.generate identity randomNextMove
        |> Cmd.map f


-- Score all available moves given the current game state
scoreMoves : GameState -> List ScoredMove
scoreMoves gameState =
    let
        minimaxScore : Move -> Int
        minimaxScore move =
            minimax (performMove gameState.currentPlayer move gameState) 1 Minimize

        -- List all potential moves
        potentialMoves =
            validMoves gameState

        -- Score the moves according to how good/bad they are using minimax
        scoredMoves =
            potentialMoves
                |> List.map (\m -> { move = m, score = (minimaxScore m) } )
                |> List.sortBy .score
                |> List.reverse
                |> Debug.log "available moves"
    in
    scoredMoves

-- Degrade the quality of the move predictions according to the difficulty level
degradeMoves : Difficulty -> List ScoredMove -> Random.Generator (List ScoredMove)
degradeMoves difficulty scoredMoves =
    let
        -- Find what's the delta between min and max score
        delta =
            scoredMoves
                |> List.map .score
                |> (\l -> Maybe.map2 (-) (List.maximum l) (List.minimum l))
                |> Maybe.withDefault 0
                |> Debug.log "delta"

        -- Determine max penalty according to difficulty
        maxPenalty = 
            Debug.log "difficulty" difficulty
                |> penaltyFactor
                |> (*) (toFloat delta)
                |> round
                |> min maxScore -- don't make penalty greater than max possible score
                |> Debug.log "max penalty"
        initialScores : Random.Generator (List ScoredMove)
        initialScores =
            Random.constant scoredMoves
        randomPenalty = 
            Random.int 0 maxPenalty
        -- Generate random penalties for each move
        randomPenalties : Random.Generator (List Int)
        randomPenalties =
            Random.list (List.length scoredMoves) randomPenalty
            |> Random.map (Debug.log "penalties")
        -- Apply the penalties
        degradedScores =
            Random.map2 (List.map2 (\m penalty -> { m | score = m.score - penalty } )) initialScores randomPenalties
            |> Random.map (List.sortBy .score >> List.reverse)
            |> Random.map (Debug.log "degraded moves")
    in
    degradedScores

pickMove : List ScoredMove -> Random.Generator (Maybe Move)
pickMove scoredMoves =
    let
        -- Find the best score
        bestScore =
            scoredMoves
                |> List.head
                |> Maybe.map .score

        -- Pick all of the moves that have the same best score
        bestMoves =
            bestScore
                |> Maybe.map (\best -> List.filter (\move -> move.score == best) scoredMoves)
                |> Debug.log "next best moves"
                |> Maybe.map (List.map .move)

        -- Select a random move from the best moves, or no moves if we can't do anything
        randomNextMove =
            case bestMoves of
                Nothing ->
                    Random.constant Nothing

                Just moves ->
                    Random.Extra.sample moves
    in
    randomNextMove


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


maxScore =
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
                    -maxScore

                Left O ->
                    maxScore

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
                                List.maximum values |> Maybe.withDefault -maxScore
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
                                List.minimum values |> Maybe.withDefault maxScore
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
    let
        coeffs = coefficients
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


coefficients : Coefficients
coefficients =
    { won = 6
    , noChanceOfWinning = 0
    , canPlaceThirdInRow = 4
    , canPlaceSecondInRow = 2
    , canPlaceFirstInRow = 1
    }

penaltyFactor : Difficulty -> Float
penaltyFactor difficulty =
    case difficulty of 
        Easy ->
            2.0
        Normal ->
            1.0
        Hard ->
            0.0
