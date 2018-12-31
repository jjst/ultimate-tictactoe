module Main exposing (..)


import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, string, top)
import Task
import Process
import Window
import Html.Attributes as HA
import Html
import Html exposing (Html, div, node, text, button)
import Html.Events exposing (onClick)
import Svg exposing (svg)
import Svg.Attributes as SA

import UltimateTicTacToe exposing (GameState, Move)
import TicTacToeBase
import SvgUtils
import AI
import GameMode
import Player exposing (Player)

import Tuple3 as T3

main =
    Navigation.program NewLocation
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL

type alias GameSettings = Maybe GameMode.Mode

type alias Model =
    { gameState : GameState
    , gameSettings : GameSettings
    , windowSize : Window.Size
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { gameState = UltimateTicTacToe.init
            , gameSettings = Nothing
            , windowSize = { width = 0, height = 0 }
            }
    in
        model ! [ getWindowSize ]


-- URL PARSING

type alias GameId = String

type Route
  = Home
  | TwoPlayersRemote GameId


route : Url.Parser (Route -> a) a
route =
  Url.oneOf
    [ Url.map Home top
    , Url.map TwoPlayersRemote (string)
    ]


-- UPDATE

type PlayerType 
    = CurrentPlayer
    | OtherPlayer

type Msg
    = PerformMove Player Move
    | NewWindowSize Window.Size
    | ChooseGameMode GameMode.Mode
    | NewLocation Navigation.Location
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ gameState, gameSettings, windowSize } as model) =
    case (Debug.log "" msg) of
        PerformMove player move -> 
            let
                newState = UltimateTicTacToe.performMove player move gameState
                cmd =
                    if player == Player.X && gameSettings == Just GameMode.OnePlayerVsAI then
                        getAIMove newState
                    else
                        Cmd.none
            in
                { model | gameState = newState } ! [ cmd ]
        NewWindowSize size ->
            { model | windowSize = size } ! []
        ChooseGameMode gameMode ->
            { model | gameSettings = Just gameMode } ! []
        NewLocation location ->
            model ! [] -- TODO
        NoOp -> 
            model ! [] -- TODO


getWindowSize : Cmd Msg
getWindowSize =
    Task.perform NewWindowSize Window.size


getAIMove : GameState -> Cmd Msg
getAIMove currentBoard =
    case AI.nextMove currentBoard of
        Just move -> 
            Process.sleep 400.0 |> Task.perform (\_ -> PerformMove Player.O move)
        Nothing ->
            Cmd.none

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes NewWindowSize ]



-- VIEW


view : Model -> Html Msg
view ({ gameState, gameSettings, windowSize } as model) =
    let
        minSize =
            ((Basics.min windowSize.width windowSize.height) |> toFloat) - 5

        size =
            (toString minSize)

        mainDivStyle =
            HA.style
                [ ( "margin", "auto" )
                , ( "position", "relative" )
                , ( "width", size ++ "px" )
                ]

        gameBoardView = viewGameState minSize gameSettings gameState

        elementsToDisplay =
            case gameSettings of
                Just _ -> [ gameBoardView ]
                Nothing -> [ viewMenu, gameBoardView ]

    in
        div [ mainDivStyle ] ([ css "style.css" ] ++ elementsToDisplay)

viewMenu : Html Msg
viewMenu =
    let
        title = div [ HA.class "menutitle" ] [ text "Ultimate tic-tac-toe" ]
        options = 
            div [] 
                [ button [ onClick (ChooseGameMode GameMode.OnePlayerVsAI) ] [ text "1 Player vs AI" ]
                , button [ onClick (ChooseGameMode GameMode.TwoPlayersLocal) ] [ text "2 Players (local)" ]
                , button [ onClick (ChooseGameMode GameMode.TwoPlayersRemote) ] [ text "2 Players (remote)" ]
                ]

        menuView =
            div [ HA.class "tutorial" ]
                [ title, options ]
        menuStyle =
            HA.style
                [ ( "z-index", "1" )
                , ( "left", "5%" )
                , ( "top", "5%" )
                , ( "position", "absolute" )
                , ( "width", "90%" )
                , ( "font-family", "'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif" )
                ]
    in
        div [ menuStyle ] [ menuView ]

viewGameState : Float -> GameSettings -> GameState -> Html Msg
viewGameState minSize gameSettings gameState =
    let
        baseBoardSize =
            TicTacToeBase.boardSize |> toFloat

        scale =
            minSize / baseBoardSize

        size =
            (toString minSize)

        msgType =
            case gameSettings of
                Just GameMode.OnePlayerVsAI -> PerformMove Player.X
                _ -> PerformMove gameState.currentPlayer

        svgView =
            UltimateTicTacToe.svgView gameState
                |> SvgUtils.scale scale
                |> Svg.map msgType

    in
        svg [ SA.viewBox ("0 0 " ++ size ++ " " ++ size), SA.width (size ++ "px") ] [ svgView ]

css : String -> Html a
css path =
    node "link" [ HA.rel "stylesheet", HA.href path ] []
