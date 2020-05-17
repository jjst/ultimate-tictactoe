module Main exposing (GameSettings, Model, Msg(..), PlayerType(..), Route(..), css, getAIMove, init, main, subscriptions, update, view, viewGameState)

import Random
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html, button, div, p, node, text, input)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Process
import Svg exposing (svg)
import Svg.Attributes as SA
import SvgUtils
import Task
import Url exposing (Url)

import Sizes
import AI
import GameMode
import TicTacToeBase
import Tuple3 as T3
import GameId
import UltimateTicTacToe exposing (GameState, Move)
import Player exposing (..)

main : Program Config Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }



-- MODEL


type GameSettings
   = NotYetSelected
   | LocalVsAI
   | Local2Players
   | Remote2Players GameId.GameId PlayState


type alias Config =
   { baseUrl : String
   , remotePlayServerUrl : String
   }


type alias Model =
    { config : Config
    , gameState : GameState
    , gameSettings : GameSettings
    , windowSize : WindowSize
    }


init : Config -> Url -> Nav.Key -> ( Model, Cmd Msg )
init conf url key =
    let
        model =
            { config = conf
            , gameState = UltimateTicTacToe.init
            , gameSettings = NotYetSelected
            , windowSize = { width = 0, height = 0 }
            }
    in
    ( model
    , getInitialWindowSize
    )


type PlayState
   = WaitingForPlayers
   | InProgress

-- URL PARSING



type Route
    = Home
    | JoinRemoteGame GameId.GameId

-- UPDATE

type alias WindowSize = { width : Int, height : Int }

type PlayerType
    = CurrentPlayer
    | OtherPlayer


type Msg
    = PerformedMove Player Move
    | NewWindowSize WindowSize
    | GeneratedRemoteGameId GameId.GameId
    | ChoseGameMode GameMode.Mode
    | CancelledRemote
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | Ignored


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ gameState, gameSettings, windowSize } as model) =
    case msg of
        PerformedMove player move ->
            let
                newState =
                    UltimateTicTacToe.performMove player move gameState

                cmd =
                    if player == Player.X && gameSettings == LocalVsAI then
                        getAIMove newState

                    else
                        Cmd.none
            in
            ( { model | gameState = newState }
            , cmd
            )

        NewWindowSize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        CancelledRemote ->
            ( { model | gameSettings = NotYetSelected, gameState = UltimateTicTacToe.init }
            , Cmd.none
            )

        GeneratedRemoteGameId gameId ->
            ( { model | gameSettings = (Remote2Players gameId WaitingForPlayers), gameState = UltimateTicTacToe.init }
            , Cmd.none
            )

        ChoseGameMode GameMode.TwoPlayersRemote ->
            ( model
            , Random.generate GeneratedRemoteGameId GameId.random
            )

        ChoseGameMode GameMode.OnePlayerVsAI ->
            ( { model | gameSettings = LocalVsAI, gameState = UltimateTicTacToe.init }
            , Cmd.none
            )

        ChoseGameMode GameMode.TwoPlayersLocal ->
            ( { model | gameSettings = Local2Players, gameState = UltimateTicTacToe.init }
            , Cmd.none
            )

        _ ->
            ( model
            , Cmd.none
            )


-- TODO


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform (\viewport -> NewWindowSize { width = round viewport.viewport.width, height = round viewport.viewport.height }) Browser.Dom.getViewport

getAIMove : GameState -> Cmd Msg
getAIMove currentBoard =
    case AI.nextMove currentBoard of
        Just move ->
            Process.sleep 1000.0 |> Task.perform (\_ -> PerformedMove Player.O move)

        Nothing ->
            Cmd.none



-- SUBSCRIPTIONS

onWindowResize : Sub Msg
onWindowResize =
    Browser.Events.onResize (\w h -> NewWindowSize { width = w, height = h })

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ onWindowResize ]



-- VIEW

prependMaybe : List a -> Maybe a -> List a
prependMaybe list maybe =
   case maybe of
           Just value ->
             value :: list
           Nothing ->
             list

view : Model -> Browser.Document Msg
view ({ config, gameState, gameSettings, windowSize } as model) =
    let
        minSize =
            (Basics.min windowSize.width windowSize.height |> toFloat) - 5

        size =
            String.fromFloat minSize

        mainDivStyles =
                [ HA.style "margin" "auto"
                , HA.style "position" "relative"
                , HA.style "width" (size ++ "px")
                , HA.style "height" (size ++ "px")
                ]

        gameBoardView =
            viewGameState minSize gameSettings gameState

        maybeMenu =
            case (gameSettings, UltimateTicTacToe.winner gameState.board) of
                (NotYetSelected, _) ->
                    Just (viewMainMenu Nothing)
                (Remote2Players gameId WaitingForPlayers, _) ->
                    let
                        gameUrl = config.baseUrl ++ "/game/" ++ gameId
                    in
                    Just (viewWaitingForPlayerMenu gameUrl)
                (_, Just winner) ->
                    Just (viewMainMenu (Just winner))
                (_, _) ->
                    Nothing

        elementsToDisplay =  prependMaybe [ gameBoardView ] maybeMenu

        html =
            div mainDivStyles ([ css "style.css" ] ++ elementsToDisplay)
    in
       { title = "Ultimate Tic-Tac-Toe"
       , body = [ html ]
       }

viewWaitingForPlayerMenu : String -> Html Msg
viewWaitingForPlayerMenu gameUrl =
    let
        title = "Waiting for players..."

        titleDiv =
            div [ HA.class "menutitle" ] [ text title ]

        mainDiv =
            div [ HA.class "buttons" ]
               [ div [ HA.class "menu-item" ]
                 [ p [] [ text "Waiting for another player" ]
                 , p [] [ text "They can join using the following link:" ]
                 ]
               , input [ HA.class "menu-item", HA.readonly True, HA.value gameUrl ] []
               , button [ HA.class "menu-item", onClick (CancelledRemote) ] [ text "Cancel" ]
               ]

        menu = div [ HA.id "menu" ] [ titleDiv, mainDiv ]

        containerClass = "fade-in"

        menuContainer = div [ HA.id "menu-container", HA.class containerClass ] [ menu ]
    in
    menuContainer

viewMainMenu : Maybe Winner -> Html Msg
viewMainMenu maybeWinner =
    let
        title =
            case maybeWinner of
                (Nothing) ->
                    "Ultimate Tic-Tac-Toe"
                Just (Right Draw) ->
                    "It's a draw! Replay:"
                Just (Left X) ->
                    "X wins! Replay:"
                Just (Left O) ->
                    "O wins! Replay:"

        titleDiv =
            div [ HA.class "menutitle" ] [ text title ]

        options =
            div [ HA.class "buttons" ]
                [ button [ HA.class "menu-item", onClick (ChoseGameMode GameMode.OnePlayerVsAI) ] [ text "1 Player vs AI" ]
                , button [ HA.class "menu-item", onClick (ChoseGameMode GameMode.TwoPlayersLocal) ] [ text "2 Players (local)" ]
                , button [ HA.class "menu-item", onClick (ChoseGameMode GameMode.TwoPlayersRemote) ] [ text "2 Players (remote)" ]
                ]

        menu = div [ HA.id "menu" ] [ titleDiv, options ]

        containerClass = if maybeWinner == Nothing then "fade-in" else "fade-in delay"

        menuContainer = div [ HA.id "menu-container", HA.class containerClass ] [ menu ]
    in
    menuContainer


viewGameState : Float -> GameSettings -> GameState -> Html Msg
viewGameState minSize gameSettings gameState =
    let
        baseBoardSize =
            Sizes.boardSize |> toFloat

        scale =
            minSize / baseBoardSize

        size =
            String.fromFloat minSize

        msgType =
            case gameSettings of
                LocalVsAI ->
                    PerformedMove Player.X

                _ ->
                    PerformedMove gameState.currentPlayer

        svgView =
            UltimateTicTacToe.svgView gameState
                |> SvgUtils.scale scale
                |> Svg.map msgType
    in
    svg [ SA.viewBox ("0 0 " ++ size ++ " " ++ size), SA.width (size ++ "px") ] [ svgView ]


css : String -> Html a
css path =
    node "link" [ HA.rel "stylesheet", HA.href path ] []
