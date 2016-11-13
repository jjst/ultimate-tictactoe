module UltimateTicTacToeWithAI exposing (..)

import UltimateTicTacToe
import TicTacToe
import Board
import Cell

-- MODEL

type alias Model = UltimateTicTacToe.Model

type alias Move =
    { boardCoords: Board.Coords
    , cellCoords: Board.Coords
    }

nextMove : UltimateTicTacToe.Model -> Maybe Move
nextMove board =
    let 
        minimaxScore : Move -> Int
        minimaxScore move = minimax (applyMove move board) 5 Maximize
        potentialMoves = validMoves board
        potentialMoves 
            |> List.sortBy minimaxScore 
            |> List.reverse 
            |> List.head

toMsg : Move -> UltimateTicTacToe.Msg
toMsg { boardCoords, cellCoords } =
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

applyMove : Move -> UltimateTicTacToe.Model -> UltimateTicTacToe.Model
applyMove move board =
    UltimateTicTacToe.update (toMsg move) board

maxValue = 100000

type Action = Maximize | Minimize

-- Assume that AI is always O for now
minimax : UltimateTicTacToe.Model -> Int -> Action -> Int
minimax board depth action =
    case UltimateTicTacToe.winner board of
        Just winner ->
            case Left X -> -maxValue
            case Left O -> maxValue
            case Right Draw -> 0
        Nothing ->
            if depth == 0 then
                evalPosition board
            else
                case action of
                    Maximize ->
                        let
                           values = validMoves board |> List.map (\move -> 
                               minimax (applyMove move board) (depth - 1) Minimize
                           )
                           max = List.maximum values |> Maybe.withDefault (-maxValue)
                        in
                          max
                    Minimize ->
                        let
                           values = validMoves board |> List.map (\move -> 
                               minimax (applyMove move board) (depth - 1) Maximize
                           )
                           min = List.minimum values |> Maybe.withDefault (maxValue)
                        in
                           min


evalPosition : UltimateTicTacToe.Model -> Int

-- UPDATE

type alias Msg = UltimateTicTacToe.Msg

update : Msg -> Model -> Model
update msg ({board, currentPlayer, currentBoardCoords} as model) =
    let
        updatedModelAfterPlayerMove = UltimateTicTacToe.update msg model
        aiMove = nextMove updatedModelAfterPlayerMove
        updatedModelAfterAIMove = UltimateTicTacToe.update (toMsg aiMove) model
    in
       updatedModelAfterAIMove

-- VIEW

view = UltimateTicTacToe.view
