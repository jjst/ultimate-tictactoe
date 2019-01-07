module Main exposing (GameId, GameSettings, Model, Msg(..), PlayerType(..), Route(..), css, getAIMove, init, main, subscriptions, update, view, viewGameState, viewMenu)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import Html exposing (Html, button, div, node, text)
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
import UltimateTicTacToe exposing (GameState, Move)
import Player exposing (Player)

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


type alias GameSettings =
    Maybe GameMode.Mode


type alias Model =
    { gameState : GameState
    , gameSettings : GameSettings
    , windowSize : WindowSize
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        model =
            { gameState = UltimateTicTacToe.init
            , gameSettings = Nothing
            , windowSize = { width = 0, height = 0 }
            }
    in
    ( model
    , getInitialWindowSize
    )



-- URL PARSING


type alias GameId =
    String


type Route
    = Home
    | TwoPlayersRemote GameId



-- UPDATE

type alias WindowSize = { width : Int, height : Int }

type PlayerType
    = CurrentPlayer
    | OtherPlayer


type Msg
    = PerformedMove Player Move
    | NewWindowSize WindowSize
    | ChoseGameMode GameMode.Mode
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url
    | Ignored


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ gameState, gameSettings, windowSize } as model) =
    case Debug.log "" msg of
        PerformedMove player move ->
            let
                newState =
                    UltimateTicTacToe.performMove player move gameState

                cmd =
                    if player == Player.X && gameSettings == Just GameMode.OnePlayerVsAI then
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

        ChoseGameMode gameMode ->
            ( { model | gameSettings = Just gameMode }
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
            Process.sleep 400.0 |> Task.perform (\_ -> PerformedMove Player.O move)

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


view : Model -> Browser.Document Msg
view ({ gameState, gameSettings, windowSize } as model) =
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

        elementsToDisplay =
            case gameSettings of
                Just _ ->
                    [ gameBoardView ]

                Nothing ->
                    [ viewMenu, gameBoardView ]
        html = 
            div mainDivStyles ([ css "style.css" ] ++ elementsToDisplay)
    in
       { title = "Ultimate Tic-Tac-Toe"
       , body = [ html ]
       }

viewMenu : Html Msg
viewMenu =
    let
        title =
            div [ HA.class "menutitle" ] [ text "Ultimate tic-tac-toe" ]

        options =
            div [ HA.class "buttons" ]
                [ button [ onClick (ChoseGameMode GameMode.OnePlayerVsAI) ] [ text "1 Player vs AI" ]
                , button [ onClick (ChoseGameMode GameMode.TwoPlayersLocal) ] [ text "2 Players (local)" ]
                , button [ onClick (ChoseGameMode GameMode.TwoPlayersRemote) ] [ text "2 Players (remote)" ]
                ]

        menu = div [ HA.id "menu" ] [ title, options ]

        menuContainer = div [ HA.id "menu-container" ] [ menu ]
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
                Just GameMode.OnePlayerVsAI ->
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
