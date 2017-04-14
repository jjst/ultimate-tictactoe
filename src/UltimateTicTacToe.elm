module UltimateTicTacToe exposing (..)

import TicTacToe
import Player exposing (..)
import Board exposing (..)
import SvgUtils
import TicTacToeBase
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
    Board TicTacToe.Model


-- MODEL


type alias Model =
    { board : UltimateTicTacToeBoard
    , currentPlayer : Player
    , currentBoardCoords : Maybe Board.Coords
    }


init : Model
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


fromString : Player -> Maybe Board.Coords -> String -> Result String Model
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
                |> List.map (List.map (TicTacToe.fromString player) >> liftResult)
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


boardOwner : TicTacToe.Model -> Maybe Player
boardOwner boardModel =
    case TicTacToe.winner boardModel.board of
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


type Msg
    = MetaPlaceMark Coords TicTacToe.Msg


update : Msg -> Model -> Model
update msg ({ board, currentPlayer, currentBoardCoords } as model) =
    let
        nextPlayer =
            opponent currentPlayer
    in
        case winner board of
            Just _ ->
                model

            Nothing ->
                case msg of
                    MetaPlaceMark boardCoords ((TicTacToe.PlaceMark cellCoords) as msg) ->
                        let
                            updatedBoard =
                                board
                                    |> indexedMap
                                        (\( i, j ) subBoard ->
                                            if boardCoords == ( i, j ) then
                                                TicTacToe.update msg subBoard
                                            else
                                                TicTacToe.update TicTacToe.TogglePlayer subBoard
                                        )

                            updatedBoardWinner =
                                TicTacToe.winner (get updatedBoard cellCoords).board

                            updatedModel =
                                { board = updatedBoard
                                , currentPlayer = nextPlayer
                                , currentBoardCoords =
                                    if updatedBoardWinner == Nothing then
                                        Just cellCoords
                                    else
                                        Nothing
                                }
                        in
                            case currentBoardCoords of
                                Nothing ->
                                    updatedModel

                                Just c ->
                                    if c == boardCoords then
                                        updatedModel
                                    else
                                        model

                    _ ->
                        model



-- VIEW


view : Model -> Html Msg
view model =
    let
        size =
            (toString TicTacToeBase.boardSize)

        divStyle =
            Html.Attributes.style
                [ ( "margin", "auto" )
                , ( "width", size ++ "px" )
                ]
    in
        div [ divStyle ]
            [ svg [ viewBox ("0 0 " ++ size ++ " " ++ size), width (size ++ "px") ]
                [ (svgView model) ]
            ]


svgView : Model -> Svg Msg
svgView model =
    let
        board =
            model.board

        vb =
            svgViewBoard model

        cells =
            g [] (flatten <| (indexedMap vb board))

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


svgViewBoard : Model -> Coords -> TicTacToe.Model -> Svg Msg
svgViewBoard model (( i, j ) as coords) ({ board, currentPlayer } as subBoardModel) =
    let
        ticTacToeView =
            TicTacToe.svgView subBoardModel

        boardWinner =
            TicTacToe.winner board

        winningMark =
            case boardWinner of
                Just (Left Player.X) ->
                    [ Cell.drawCross |> SvgUtils.scale ((toFloat TicTacToeBase.boardSize) / 100.0) ]

                Just (Left Player.O) ->
                    [ Cell.drawCircle |> SvgUtils.scale ((toFloat TicTacToeBase.boardSize) / 100.0) ]

                _ ->
                    []

        s =
            TicTacToeBase.boardSize |> toString

        o =
            boardOpacity model coords subBoardModel |> toString

        group =
            g [] (winningMark ++ [ g [ opacity o ] [ ticTacToeView ] ])
    in
        group
            |> SvgUtils.scale (1.0 / 3.0)
            |> SvgUtils.translate ((T3.toInt i) * TicTacToeBase.cellSize) ((T3.toInt j) * TicTacToeBase.cellSize)
            |> Html.map (MetaPlaceMark coords)


boardOpacity : Model -> Coords -> TicTacToe.Model -> Float
boardOpacity model subBoardCoords subBoardModel =
    let
        boardWinner =
            TicTacToe.winner subBoardModel.board

        fadedOutValue =
            0.25

        normalValue =
            1.0
    in
        case winner model.board of
            Just _ ->
                fadedOutValue

            _ ->
                case model.currentBoardCoords of
                    Just cds ->
                        if (subBoardCoords /= cds) then
                            fadedOutValue
                        else
                            normalValue

                    Nothing ->
                        case boardWinner of
                            Just _ ->
                                fadedOutValue

                            Nothing ->
                                normalValue
