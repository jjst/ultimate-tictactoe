module UltimateTicTacToe exposing (GameState, Move, Opacity, UltimateTicTacToeBoard, boardOwner, fadedOutOpacity, fromString, init, isValidMove, moveIsInCurrentBoard, normalOpacity, performMove, performMoveFor, renderTicTacToeBoard, svgView, transpose, winner)

import Board exposing (..)
import Cell
import Html exposing (Html, button, div, text)
import Html.Attributes
import Html.Events exposing (onClick)
import List as L
import Player exposing (..)
import Regex
import Sizes
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SvgUtils
import TicTacToe exposing (TicTacToeBoard)
import TicTacToeBase exposing (grid, strikeThrough)
import Tuple3 as T3


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
            list |> L.foldr (\result listResult -> listResult |> Result.andThen (\items -> Result.map (\a -> (::) a items) result)) (Ok [])

        horizontalSeparator : Regex.Regex
        horizontalSeparator =
            Maybe.withDefault Regex.never <|
                Regex.fromString "-.+"

        subBoardsAsStrings : List (List String)
        subBoardsAsStrings =
            str
                |> Regex.split horizontalSeparator
                |> List.map (String.trim >> String.lines >> List.map (String.trim >> String.split "|") >> transpose >> List.map (String.join "\n"))

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

        Just theWinner ->
            case theWinner of
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
            get move.boardCoords board

        ticTacToeCell =
            get move.cellCoords ticTacToeBoard
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
                TicTacToe.winner (get cellCoords updatedBoard)
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
    0.15


svgView : (Move -> msg) -> Maybe Player -> GameState -> Svg msg
svgView f playingAs ({ board, currentPlayer } as model) =
    let
        ghost =
            case playingAs of
                Nothing -> Nothing
                Just p -> if (currentPlayer == p) then (Just p) else Nothing
        cells =
            g [] (flatten <| indexedMap (renderTicTacToeBoard f ghost model) board)

        st =
            case ( winningRow boardOwner board, winner board ) of
                ( Just ( first, middle, last ), Just theWinner ) ->
                    case theWinner of
                        Left Player.O ->
                            [ strikeThrough "red" Sizes.cellSize first last ]

                        Left Player.X ->
                            [ strikeThrough "blue" Sizes.cellSize first last ]

                        _ ->
                            []

                _ ->
                    []
    in
    g [] ([ cells, grid Sizes.cellSize ] ++ st)


renderTicTacToeBoard : (Move -> msg) -> Maybe Player -> GameState -> Coords -> TicTacToeBoard -> Svg msg
renderTicTacToeBoard f maybePlayingAs model (( i, j ) as coords) ticTacToeBoard =
    let
        boardIsOutOfPlay = isOutOfPlay model coords ticTacToeBoard

        ghost =
            if boardIsOutOfPlay then
               Nothing
            else
               maybePlayingAs
        renderedBoard =
            TicTacToe.render (\cellCoords -> f { boardCoords = coords, cellCoords = cellCoords }) ghost ticTacToeBoard

        boardWinner =
            TicTacToe.winner ticTacToeBoard

        winningMark =
            case boardWinner of
                Just (Left Player.X) ->
                    [ Cell.drawCross |> SvgUtils.scale (toFloat Sizes.boardSize / 100.0) ]

                Just (Left Player.O) ->
                    [ Cell.drawCircle |> SvgUtils.scale (toFloat Sizes.boardSize / 100.0) ]

                _ ->
                    []

        boardOpacity =
            if boardIsOutOfPlay then
                fadedOutOpacity

            else
                normalOpacity

        group =
            g [] (winningMark ++ [ g [ opacity (String.fromFloat boardOpacity) ] [ renderedBoard ] ])
    in
    group
        |> SvgUtils.scale (1.0 / 3.0)
        |> SvgUtils.translate (toFloat (T3.toInt i * Sizes.cellSize)) (toFloat (T3.toInt j * Sizes.cellSize))


isOutOfPlay : GameState -> Coords -> TicTacToeBoard -> Bool
isOutOfPlay model boardCoords ticTacToeBoard =
    case winner model.board of
        Just _ ->
            True

        _ ->
            case model.currentBoardCoords of
                Just cds ->
                    boardCoords /= cds

                Nothing ->
                    TicTacToe.winner ticTacToeBoard /= Nothing
