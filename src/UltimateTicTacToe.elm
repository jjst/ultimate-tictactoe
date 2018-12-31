module UltimateTicTacToe exposing (..)

import TicTacToe
import TicTacToe exposing (TicTacToeBoard)
import Player exposing (..)
import Board exposing (..)
import SvgUtils
import TicTacToeBase exposing (strikeThrough, cellSize, boardSize, grid)
import Cell
import Regex
import String
import List as L
import Tuple3 as T3
import Html
import Html.Attributes
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


type alias UltimateTicTacToeBoard =
    Board TicTacToeBoard



-- MODEL


type alias GameState =
    { board : UltimateTicTacToeBoard
    , currentPlayer : Player
    , currentBoardCoords : Maybe Board.Coords
    }


type alias Move =
    { boardCoords : Board.Coords
    , cellCoords : Board.Coords
    }


init : GameState
init =
    { board = T3.fill (T3.fill TicTacToe.init)
    , currentPlayer = X
    , currentBoardCoords = Nothing
    }


transpose : List (List a) -> List (List a)
transpose ll =
    case ll of
        [] ->
            []

        [] :: xss ->
            transpose xss

        (x :: xs) :: xss ->
            let
                heads =
                    List.filterMap List.head xss

                tails =
                    List.filterMap List.tail xss
            in
                (x :: heads) :: transpose (xs :: tails)


fromString : Player -> Maybe Board.Coords -> String -> Result String GameState
fromString player currentBoardCoords str =
    let
        liftResult : List (Result a b) -> Result a (List b)
        liftResult list =
            list |> L.foldr (\result listResult -> listResult |> Result.andThen (\list -> Result.map (flip (::) list) result)) (Ok [])

        subBoardsAsStrings : List (List String)
        subBoardsAsStrings =
            str
                |> Regex.split Regex.All (Regex.regex "-.+")
                |> List.map (String.trim >> String.lines >> List.map (String.trim >> String.split ("|")) >> transpose >> List.map (String.join "\n"))

        subBoards =
            subBoardsAsStrings
                |> List.map (List.map TicTacToe.fromString >> liftResult)
                |> liftResult

        boardResult =
            Result.map
                (L.map
                    (T3.fromList >> Result.fromMaybe "Wrong number of items in row")
                )
                subBoards
                |> Result.andThen liftResult
                |> Result.andThen (T3.fromList >> Result.fromMaybe "Wrong number of rows")
    in
        boardResult
            |> Result.map
                (\b ->
                    { board = b
                    , currentBoardCoords = currentBoardCoords
                    , currentPlayer = player
                    }
                )


boardOwner : TicTacToeBoard -> Maybe Player
boardOwner board =
    case TicTacToe.winner board of
        Nothing ->
            Nothing

        Just winner ->
            case winner of
                Left player ->
                    Just player

                Right Draw ->
                    Nothing


winner : UltimateTicTacToeBoard -> Maybe Winner
winner =
    Board.winner boardOwner



-- UPDATE


isValidMove : Move -> GameState -> Bool
isValidMove move ({ board, currentBoardCoords } as model) =
    let
        ticTacToeBoard =
            get board move.boardCoords

        ticTacToeCell =
            get ticTacToeBoard move.cellCoords
    in
        moveIsInCurrentBoard move model && TicTacToe.winner ticTacToeBoard == Nothing && ticTacToeCell == Nothing


moveIsInCurrentBoard : Move -> GameState -> Bool
moveIsInCurrentBoard move model =
    case model.currentBoardCoords of
        Nothing ->
            True

        Just c ->
            c == move.boardCoords


performMoveFor : Player -> Move -> UltimateTicTacToeBoard -> UltimateTicTacToeBoard
performMoveFor player { boardCoords, cellCoords } board =
    board
        |> indexedMap
            (\( i, j ) subBoard ->
                if boardCoords == ( i, j ) then
                    TicTacToe.performMoveFor player cellCoords subBoard
                else
                    subBoard
            )


performMove : Player -> Move -> GameState -> GameState
performMove player ({ boardCoords, cellCoords } as move) ({ board, currentPlayer, currentBoardCoords } as model) =
    if player == currentPlayer && isValidMove move model && (winner board == Nothing) then
        let
            nextPlayer =
                opponent currentPlayer

            updatedBoard =
                performMoveFor currentPlayer move board

            updatedBoardWinner =
                TicTacToe.winner (get updatedBoard cellCoords)
        in
            { board = updatedBoard
            , currentPlayer = nextPlayer
            , currentBoardCoords =
                if updatedBoardWinner == Nothing then
                    Just cellCoords
                else
                    Nothing
            }
    else
        model



-- VIEW


type alias Opacity =
    Float


normalOpacity : Opacity
normalOpacity =
    1.0


fadedOutOpacity : Opacity
fadedOutOpacity =
    0.25


svgView : GameState -> Svg Move
svgView ({ board } as model) =
    let
        cells =
            g [] (flatten <| (indexedMap (renderTicTacToeBoard model) board))

        st =
            case ( winningRow boardOwner board, winner board ) of
                ( Just ( first, middle, last ), Just winner ) ->
                    case winner of
                        Left Player.O ->
                            [ strikeThrough "red" cellSize first last ]

                        Left Player.X ->
                            [ strikeThrough "blue" cellSize first last ]

                        _ ->
                            []

                _ ->
                    []
    in
        g [] ([ cells, (grid cellSize) ] ++ st)


renderTicTacToeBoard : GameState -> Coords -> TicTacToeBoard -> Svg Move
renderTicTacToeBoard model (( i, j ) as coords) ticTacToeBoard =
    let
        renderedBoard =
            TicTacToe.render ticTacToeBoard

        boardWinner =
            TicTacToe.winner ticTacToeBoard

        winningMark =
            case boardWinner of
                Just (Left Player.X) ->
                    [ Cell.drawCross |> SvgUtils.scale ((toFloat boardSize) / 100.0) ]

                Just (Left Player.O) ->
                    [ Cell.drawCircle |> SvgUtils.scale ((toFloat boardSize) / 100.0) ]

                _ ->
                    []

        boardOpacity =
            if shouldFadeOut model coords ticTacToeBoard then
                fadedOutOpacity
            else
                normalOpacity

        group =
            g [] (winningMark ++ [ g [ opacity (toString boardOpacity) ] [ renderedBoard ] ])
    in
        group
            |> SvgUtils.scale (1.0 / 3.0)
            |> SvgUtils.translate ((T3.toInt i) * cellSize) ((T3.toInt j) * cellSize)
            |> Svg.map (\cellCoords -> { boardCoords = coords, cellCoords = cellCoords })


shouldFadeOut : GameState -> Coords -> TicTacToeBoard -> Bool
shouldFadeOut model boardCoords ticTacToeBoard =
    case winner model.board of
        Just _ ->
            True

        _ ->
            case model.currentBoardCoords of
                Just cds ->
                    boardCoords /= cds

                Nothing ->
                    TicTacToe.winner ticTacToeBoard /= Nothing
