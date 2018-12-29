module Main exposing (..)


import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, string, top)
import Task
import Window
import Html.Attributes as HA
import Html
import Html exposing (Html, div, node, text, button)
import Html.Events exposing (onClick)
import Svg exposing (svg)
import Svg.Attributes as SA

import UltimateTicTacToe
import TicTacToeBase
import SvgUtils
import AI
import GameMode

main =
    Navigation.program NewLocation
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL

type alias GameSettings = Maybe GameMode.Mode

type alias GameBoard = UltimateTicTacToe.Model

type alias Model =
    { gameBoard : GameBoard
    , gameSettings : GameSettings
    , windowSize : Window.Size
    }


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        l = location |> Debug.log "location"
        r = Url.parsePath route location |> Debug.log "parsed path"
        model =
            { gameBoard = UltimateTicTacToe.init
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


type Msg
    = GameMessage UltimateTicTacToe.Msg
    | NewWindowSize Window.Size
    | ChooseGameMode GameMode.Mode
    | NewLocation Navigation.Location


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ gameBoard, gameSettings, windowSize } as model) =
    let
        newModel =
            case msg of
                GameMessage msg -> 
                    { model | gameBoard = updateBoard gameSettings msg gameBoard }
                NewWindowSize size ->
                    { model | windowSize = size }
                ChooseGameMode gameMode ->
                    { model | gameSettings = Just gameMode }
                NewLocation location ->
                    model -- TODO
    in
        newModel ! []

updateBoard : GameSettings -> UltimateTicTacToe.Msg -> GameBoard -> GameBoard
updateBoard settings msg model =
    let
        updatedBoard = UltimateTicTacToe.update msg model
    in
        case settings of
            Just GameMode.OnePlayerVsAI ->
                AI.nextMove updatedBoard 
                  |> Maybe.map (\move -> UltimateTicTacToe.update (UltimateTicTacToe.PerformMove move) updatedBoard)
                  |> Maybe.withDefault updatedBoard

            _ ->
                updatedBoard



getWindowSize : Cmd Msg
getWindowSize =
    Task.perform NewWindowSize Window.size



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes NewWindowSize ]



-- VIEW


view : Model -> Html Msg
view ({ gameBoard, gameSettings, windowSize } as model) =
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

        gameBoardView = viewGameBoard minSize gameBoard

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


viewGameBoard : Float -> GameBoard -> Html Msg
viewGameBoard minSize board =
    let
        baseBoardSize =
            TicTacToeBase.boardSize |> toFloat

        scale =
            minSize / baseBoardSize

        size =
            (toString minSize)

        svgView =
            UltimateTicTacToe.svgView board
                |> SvgUtils.scale scale
                |> Html.map GameMessage
    in
        svg [ SA.viewBox ("0 0 " ++ size ++ " " ++ size), SA.width (size ++ "px") ] [ svgView ]

css : String -> Html a
css path =
    node "link" [ HA.rel "stylesheet", HA.href path ] []
