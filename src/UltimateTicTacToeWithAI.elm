module UltimateTicTacToeWithAI exposing (..)

import UltimateTicTacToe
import TicTacToe
import Board
import Cell
import Player exposing (..)

import Debug

-- MODEL

type alias Model = UltimateTicTacToe.Model

type alias Move =
    { boardCoords: Board.Coords
    , cellCoords: Board.Coords
    }

nextMove : Model -> Maybe Move
nextMove board =
    let
        minimaxScore : Move -> Int
        minimaxScore move = minimax (applyMove move board) 2 Maximize
        potentialMoves = validMoves board
        scoredMoves = potentialMoves |> List.map (\m -> (m, minimaxScore m))
        nextMove = scoredMoves
            |> List.sortBy snd
            |> List.reverse
            |> List.head
            |> Maybe.map fst
    in
       Debug.log "next move" nextMove

toMsg : Move -> UltimateTicTacToe.Msg
toMsg { boardCoords, cellCoords } =
    UltimateTicTacToe.MetaPlaceMark boardCoords (TicTacToe.PlaceMark cellCoords Cell.PlaceMark)

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

applyMove : Move -> Model -> Model
applyMove move board =
    UltimateTicTacToe.update (toMsg move) board

maxValue = 100000

type Action = Maximize | Minimize

-- Assume that AI is always O for now
minimax : Model -> Int -> Action -> Int
minimax ticTacToe depth action =
    case UltimateTicTacToe.winner ticTacToe.board of
        Just winner ->
            case winner of
                Left X -> -maxValue
                Left O -> maxValue
                Right Draw -> 0
        Nothing ->
            if depth == 0 then
                evalPosition ticTacToe
            else
                case action of
                    Maximize ->
                        let
                           values = validMoves ticTacToe |> List.map (\move ->
                               minimax (applyMove move ticTacToe) (depth - 1) Minimize
                           )
                           max = List.maximum values |> Maybe.withDefault (-maxValue)
                        in
                          max
                    Minimize ->
                        let
                           values = validMoves ticTacToe |> List.map (\move ->
                               minimax (applyMove move ticTacToe) (depth - 1) Maximize
                           )
                           min = List.minimum values |> Maybe.withDefault (maxValue)
                        in
                           min


evalPosition : Model -> Int
evalPosition m = 0

-- UPDATE

type alias Msg = UltimateTicTacToe.Msg

update : Msg -> Model -> Model
update msg ({board, currentPlayer, currentBoardCoords} as model) =
    let
        updatedModelAfterPlayerMove = UltimateTicTacToe.update msg model
        aiMove = nextMove updatedModelAfterPlayerMove
        updatedModelAfterAIMove =
            case aiMove of
                Just move -> UltimateTicTacToe.update (toMsg move) updatedModelAfterPlayerMove
                Nothing -> updatedModelAfterPlayerMove
    in
       updatedModelAfterAIMove

-- VIEW

view = UltimateTicTacToe.view
