module UltimateTicTacToeWithAI exposing (..)

import Tuple3
import UltimateTicTacToe
import TicTacToe
import Board
import Player exposing (..)
import Debug


-- MODEL


type alias Model =
    UltimateTicTacToe.Model


type alias Move =
    { boardCoords : Board.Coords
    , cellCoords : Board.Coords
    }


nextMove : Model -> Maybe Move
nextMove board =
    let
        minimaxScore : Move -> Int
        minimaxScore move =
            minimax (applyMove move board) 1 Minimize

        potentialMoves =
            validMoves board

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


toMsg : Move -> UltimateTicTacToe.Msg
toMsg { boardCoords, cellCoords } =
    UltimateTicTacToe.MetaPlaceMark boardCoords (TicTacToe.PlaceMark cellCoords)


-- List all valid moves given current state of the game


validMoves : Model -> List Move
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


validMovesOnBoard : TicTacToe.Model -> List Board.Coords
validMovesOnBoard ticTacToe =
    case TicTacToe.winner ticTacToe.board of
        Nothing ->
            ticTacToe.board
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


applyMove : Move -> Model -> Model
applyMove move board =
    UltimateTicTacToe.update (toMsg move) board


maxValue =
    100000


type Action
    = Maximize
    | Minimize



-- Assume that AI is always O for now


minimax : Model -> Int -> Action -> Int
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
                                            minimax (applyMove move ticTacToe) (depth - 1) Minimize
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
                                            minimax (applyMove move ticTacToe) (depth - 1) Maximize
                                        )

                            min =
                                List.minimum values |> Maybe.withDefault (maxValue)
                        in
                            min



-- Heuristic to evaluate the current board


evalPosition : Model -> Int
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


winnableBoards : Model -> Player -> Int
winnableBoards model player =
    model.board
        |> Board.flatten
        |> List.filter (\b -> isWinnable b player)
        |> List.length



-- A board is winnable if it has 2 in a row


isWinnable : TicTacToe.Model -> Player -> Bool
isWinnable ticTacToe player =
    let
        isWinnable : List (Maybe Player) -> Bool
        isWinnable row =
            let
                markCount =
                    row |> List.filter (\e -> e == Just player) |> List.length
            in
                markCount == 2 && List.member Nothing row

        board =
            ticTacToe.board

        rows : List (List (Maybe Player))
        rows =
            Board.allRows
                |> List.map (\row -> row |> Tuple3.toList |> List.map (\coords -> (Board.get board coords)))
    in
        List.any isWinnable rows


wonBoards : Model -> Player -> Int
wonBoards model player =
    let
        subBoards =
            Board.flatten model.board

        wonBoards =
            subBoards |> List.filter (\b -> TicTacToe.winner b.board == Just (Left player)) |> List.length
    in
        wonBoards



-- UPDATE


type alias Msg =
    UltimateTicTacToe.Msg


update : Msg -> Model -> Model
update msg ({ board, currentPlayer, currentBoardCoords } as model) =
    let
        updatedModelAfterPlayerMove =
            UltimateTicTacToe.update msg model

        aiMove =
            nextMove updatedModelAfterPlayerMove

        updatedModelAfterAIMove =
            case aiMove of
                Just move ->
                    UltimateTicTacToe.update (toMsg move) updatedModelAfterPlayerMove

                Nothing ->
                    updatedModelAfterPlayerMove
    in
        updatedModelAfterAIMove



-- VIEW


view =
    UltimateTicTacToe.view
