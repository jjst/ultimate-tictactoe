module Main exposing (main)

import AI
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import GameId
import GameMode
import GameServer
import Html exposing (Html, button, div, input, node, p, text)
import Html.Attributes as HA
import Html.Events exposing (onClick)
import Http
import Player exposing (..)
import Process
import Random
import Sizes
import Svg exposing (svg)
import Svg.Attributes as SA
import SvgUtils
import Task
import TicTacToeBase
import Tuple3 as T3
import Tutorial
import UltimateTicTacToe exposing (GameState, Move)
import Url
import Url.Builder as UrlBuilder
import Url.Parser as UrlParser
import UrlUtils


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
    | ViewingTutorial Tutorial.Model
    | Error String
    | LocalVsAI SinglePlayerState
    | Local2Players
    | Remote2Players GameId.GameId Player.Player RemoteState

type alias Config =
    { remotePlayServerUrl : String
    }


type alias Model =
    { baseUrl : Url.Url
    , navigationKey : Nav.Key
    , config : Config
    , gameState : GameState
    , gameSettings : GameSettings
    , windowSize : WindowSize
    }


type SinglePlayerState
    = ChoosingDifficulty
    | WaitingForAI AI.Difficulty
    | Playing AI.Difficulty


type RemoteState
    = Creating
    | Joining
    | RemoteError RemoteProblem
    | WaitingForPlayers
    | InProgress


type RemoteProblem
    = Expected String
    | UnexpectedHttpError Http.Error
    | UnexpectedOther String


init : Config -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init conf url key =
    let
        ( gameSettings_, msgs ) =
            case route url of
                Home ->
                    ( NotYetSelected
                    , []
                    )

                JoinRemoteGame gameId ->
                    let
                        player =
                            Player.O
                    in
                    ( Remote2Players gameId player Joining
                    , [ GameServer.joinGame (\e -> RemoteGameMsg gameId player (RemoteGameJoined e)) conf.remotePlayServerUrl gameId Player.O ]
                    )

        model =
            { baseUrl = UrlUtils.baseUrl url
            , navigationKey = key
            , config = conf
            , gameState = UltimateTicTacToe.init
            , gameSettings = gameSettings_
            , windowSize = { width = 0, height = 0 }
            }
    in
    ( model
    , Cmd.batch (getInitialWindowSize :: msgs)
    )



-- URL PARSING


type Route
    = Home
    | JoinRemoteGame GameId.GameId


urlParser : UrlParser.Parser (Route -> a) a
urlParser =
    UrlParser.oneOf
        [ UrlParser.map Home UrlParser.top
        , UrlParser.map JoinRemoteGame UrlParser.string
        ]


route : Url.Url -> Route
route url =
    Maybe.withDefault Home (UrlParser.parse urlParser url)



-- UPDATE


type alias WindowSize =
    { width : Int, height : Int }


type PlayerType
    = CurrentPlayer
    | OtherPlayer


type Msg
    = PerformedMove Player Move
    | WaitedForAI
    | NewWindowSize WindowSize
    | RemoteGameMsg GameId.GameId Player RemoteMsg
    | ChoseMainMenuOption MainMenuOption
    | ChoseDifficulty AI.Difficulty
    | RequestedMainMenu
    | ClickedLink Browser.UrlRequest
    | ChangedUrl Url.Url
    | Ignored
    | InputMsg String

type MainMenuOption
    = Play GameMode.Mode
    | WatchTutorial

type RemoteMsg
    = RemoteGameIdCreated
    | RemoteGameCreated GameServer.CreateGameResult
    | RemoteGameJoined GameServer.JoinGameResult
    | RemoteGameReceivedEvent GameServer.RemoteGameEvent


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ config, gameState, gameSettings, windowSize } as model) =
    case msg of
        WaitedForAI ->
            case gameSettings of
                LocalVsAI (WaitingForAI difficulty) ->
                    ( model
                    , getAIMove difficulty gameState
                    )
                _ ->
                    ( model
                    , Cmd.none
                    )

        PerformedMove player move ->
            -- FIXME: this branch is in need of a cleanup!
            case gameSettings of
                Remote2Players gameId thisPlayer InProgress ->
                    if thisPlayer == player then
                        ( model
                        , GameServer.playMove config.remotePlayServerUrl gameId player move
                        )

                    else
                        ( model
                        , Cmd.none
                        )

                LocalVsAI (Playing difficulty) ->
                    let
                        newState =
                            UltimateTicTacToe.performMove player move gameState

                    in
                        if player == Player.X && (UltimateTicTacToe.winner newState.board == Nothing) then
                            ( { model | gameState = newState, gameSettings = LocalVsAI (WaitingForAI difficulty) }
                            , Process.sleep 2000.0 |> Task.perform (\_ -> WaitedForAI)
                            )
                        else
                            ( { model | gameState = newState }
                            , Cmd.none
                            )
                LocalVsAI (WaitingForAI difficulty) ->
                    let
                        newState =
                            UltimateTicTacToe.performMove player move gameState
                    in
                    ( { model | gameState = newState, gameSettings = LocalVsAI (Playing difficulty) }
                    , Cmd.none
                    )

                _ ->
                    let
                        newState =
                            UltimateTicTacToe.performMove player move gameState

                        cmd =
                            Cmd.none
                    in
                    ( { model | gameState = newState }
                    , cmd
                    )

        NewWindowSize size ->
            ( { model | windowSize = size }
            , Cmd.none
            )

        RequestedMainMenu ->
            ( { model | gameSettings = NotYetSelected }
            , Nav.replaceUrl model.navigationKey (UrlBuilder.absolute [] [])
            )

        ChoseMainMenuOption (Play gameMode) ->
            chooseGameMode gameMode model

        ChoseMainMenuOption WatchTutorial ->
            ( { model | gameSettings = (ViewingTutorial Tutorial.init) }
            , Cmd.none
            )

        ChoseDifficulty difficulty ->
            chooseDifficulty difficulty model

        RemoteGameMsg gameId player message ->
            handleRemoteMessage gameId player message model

        _ ->
            ( model
            , Cmd.none
            )


handleRemoteMessage : GameId.GameId -> Player -> RemoteMsg -> Model -> ( Model, Cmd Msg )
handleRemoteMessage gameId player message ({ config, gameSettings } as model) =
    case message of
        RemoteGameIdCreated ->
            ( { model | gameSettings = Remote2Players gameId player Creating }
            , GameServer.createGame (\e -> RemoteGameMsg gameId player (RemoteGameCreated e)) config.remotePlayServerUrl gameId
            )

        RemoteGameCreated GameServer.Success ->
            ( { model | gameSettings = Remote2Players gameId player Joining }
            , GameServer.joinGame (\e -> RemoteGameMsg gameId player (RemoteGameJoined e)) config.remotePlayServerUrl gameId Player.X
            )

        RemoteGameCreated (GameServer.Problem GameServer.AlreadyExistsError) ->
            ( model
            , GameServer.generateGameId (\e -> RemoteGameMsg gameId player RemoteGameIdCreated)
            )

        RemoteGameCreated (GameServer.UnexpectedError error) ->
            Debug.log ("Oh no! We got an unexpected error communicating with the remote play server @ " ++ config.remotePlayServerUrl) error
                |> (\_ ->
                        ( { model | gameSettings = Remote2Players gameId player (RemoteError (UnexpectedHttpError error)) }
                        , Cmd.none
                        )
                   )

        RemoteGameJoined (GameServer.UnexpectedError error) ->
            Debug.log ("Oh no! We got an unexpected error communicating with the remote play server @ " ++ config.remotePlayServerUrl) error
                |> (\_ ->
                        ( { model | gameSettings = Remote2Players gameId player (RemoteError (UnexpectedHttpError error)) }
                        , Cmd.none
                        )
                   )

        RemoteGameJoined GameServer.Success ->
            ( { model | gameSettings = Remote2Players gameId player WaitingForPlayers }
            , Cmd.none
            )

        RemoteGameJoined (GameServer.Problem GameServer.GameFullError) ->
            ( { model | gameSettings = Remote2Players gameId player (RemoteError (Expected "You cannot join this game, it's already full!")) }
            , Cmd.none
            )

        RemoteGameJoined (GameServer.Problem GameServer.NotSupportedYet) ->
            ( { model | gameSettings = Remote2Players gameId player (RemoteError (Expected "Remote multiplayer isn't supported yet... come back soon :-)")) }
            , Cmd.none
            )

        RemoteGameJoined (GameServer.Problem GameServer.GameDoesNotExist) ->
            ( { model | gameSettings = Remote2Players gameId player (RemoteError (Expected "Sorry, this game does not exist!")) }
            , Cmd.none
            )

        RemoteGameReceivedEvent (GameServer.Error error) ->
            Debug.log ("Oh no! We got an unexpected error communicating with the remote play server @ " ++ config.remotePlayServerUrl) error
                |> (\_ ->
                        ( { model | gameSettings = Remote2Players gameId player (RemoteError (UnexpectedOther error)) }
                        , Cmd.none
                        )
                   )

        RemoteGameReceivedEvent (GameServer.PlayerJoined Player.X) ->
            -- We successfully joined the game!
            ( { model | gameSettings = Remote2Players gameId player WaitingForPlayers }
            , Nav.replaceUrl model.navigationKey (UrlBuilder.absolute [ gameId ] [])
            )

        RemoteGameReceivedEvent GameServer.GameStarted ->
            ( { model | gameSettings = Remote2Players gameId player InProgress }
            , Cmd.none
            )

        RemoteGameReceivedEvent (GameServer.PlayerMove p move) ->
            ( { model | gameState = UltimateTicTacToe.performMove p move model.gameState }
            , Cmd.none
            )

        RemoteGameReceivedEvent otherEvent ->
            Debug.log "Received unprocessed game event" otherEvent
                |> (\_ ->
                        ( model
                        , Cmd.none
                        )
                   )


chooseGameMode : GameMode.Mode -> Model -> ( Model, Cmd Msg )
chooseGameMode mode model =
    case mode of
        GameMode.OnePlayerVsAI ->
            ( { model | gameSettings = LocalVsAI ChoosingDifficulty, gameState = UltimateTicTacToe.init }
            , Cmd.none
            )

        GameMode.TwoPlayersLocal ->
            ( { model | gameSettings = Local2Players, gameState = UltimateTicTacToe.init }
            , Cmd.none
            )

        GameMode.TwoPlayersRemote ->
            ( model
            , GameServer.generateGameId (\gameId -> RemoteGameMsg gameId Player.X RemoteGameIdCreated)
            )


chooseDifficulty : AI.Difficulty -> Model -> ( Model, Cmd Msg )
chooseDifficulty difficulty model =
    ( { model | gameSettings = LocalVsAI (Playing difficulty), gameState = UltimateTicTacToe.init }
    , Cmd.none
    )


getInitialWindowSize : Cmd Msg
getInitialWindowSize =
    Task.perform (\viewport -> NewWindowSize { width = round viewport.viewport.width, height = round viewport.viewport.height }) Browser.Dom.getViewport


getAIMove : AI.Difficulty -> GameState -> Cmd Msg
getAIMove difficulty currentBoard =
    AI.nextMove moveOrIgnore difficulty currentBoard


moveOrIgnore : Maybe Move -> Msg
moveOrIgnore maybeMove =
    case maybeMove of
        Just move ->
            PerformedMove Player.O move

        Nothing ->
            Ignored



-- SUBSCRIPTIONS


onWindowResize : Sub Msg
onWindowResize =
    Browser.Events.onResize (\w h -> NewWindowSize { width = w, height = h })


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.gameSettings of
        Remote2Players gameId player _ ->
            Sub.batch
                [ onWindowResize
                , GameServer.onRemoteGameEvent gameId (\e -> RemoteGameMsg gameId player (RemoteGameReceivedEvent e))
                ]

        _ ->
            onWindowResize



-- VIEW


prependMaybe : List a -> Maybe a -> List a
prependMaybe list maybe =
    case maybe of
        Just value ->
            value :: list

        Nothing ->
            list


view : Model -> Browser.Document Msg
view ({ baseUrl, config, gameState, gameSettings, windowSize } as model) =
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
        elementsToDisplay =
            case gameSettings of
                ViewingTutorial m -> [ Tutorial.view m ]
                other -> viewMainElements model

        html =
            div mainDivStyles ([ css "style.css" ] ++ elementsToDisplay)
    in
    { title = "Ultimate Tic-Tac-Toe"
    , body = [ html ]
    }

viewMainElements : Model -> List (Html Msg) 
viewMainElements ({ baseUrl, config, gameState, gameSettings, windowSize } as model) =
    let
        minSize =
            (Basics.min windowSize.width windowSize.height |> toFloat) - 5

        gameBoardView =
            viewGameState minSize gameSettings gameState

        maybeMenu =
            case ( gameSettings, UltimateTicTacToe.winner gameState.board ) of
                ( NotYetSelected, _ ) ->
                    Just (viewMainMenu Nothing)

                ( LocalVsAI ChoosingDifficulty, _ ) ->
                    Just viewChooseDifficultyMenu

                ( Remote2Players gameId _ (RemoteError error), _ ) ->
                    Just (viewError error)

                ( Remote2Players gameId player Creating, _ ) ->
                    Just (viewWaitingForPlayerMenu Nothing player)

                ( Remote2Players gameId player Joining, _ ) ->
                    Just (viewWaitingForPlayerMenu Nothing player)

                ( Remote2Players gameId player WaitingForPlayers, _ ) ->
                    let
                        gameUrl =
                            { baseUrl | path = "/" ++ gameId }
                    in
                    Just (viewWaitingForPlayerMenu (Just gameUrl) player)

                ( _, Just winner ) ->
                    Just (viewMainMenu (Just winner))

                ( _, _ ) ->
                    Nothing

        elementsToDisplay =
            prependMaybe [ gameBoardView ] maybeMenu
    in
    elementsToDisplay


    

viewError : RemoteProblem -> Html Msg
viewError error =
    let
        title =
            "Woops..."

        titleDiv =
            div [ HA.class "menutitle" ] [ text title ]

        errorMessage =
            case error of
                Expected e ->
                    e

                _ ->
                    "Error communicating with the server, cannot play remotely :-("

        mainDiv =
            div [ HA.class "buttons" ]
                [ div [ HA.class "menu-item" ]
                    [ p [] [ text errorMessage ]
                    ]
                , button [ HA.class "menu-item", onClick RequestedMainMenu ] [ text "Back to main menu" ]
                ]

        menu =
            div [ HA.id "menu" ] [ titleDiv, mainDiv ]

        containerClass =
            "fade-in"

        menuContainer =
            div [ HA.id "menu-container", HA.class containerClass ] [ menu ]
    in
    menuContainer


viewWaitingForPlayerMenu : Maybe Url.Url -> Player.Player -> Html Msg
viewWaitingForPlayerMenu maybeGameUrl player =
    let
        title =
            "Waiting for players..."

        titleDiv =
            div [ HA.class "menutitle" ] [ text title ]

        mainDiv =
            case maybeGameUrl of
                Just gameUrl ->
                    div [ HA.class "buttons" ]
                        [ div [ HA.class "menu-item" ]
                            [ p [] [ text ("Waiting for another player. You'll be playing as '" ++ Player.toString player ++ "'") ]
                            , p [] [ text "They can join using the following link:" ]
                            ]
                        , input [ HA.class "menu-item", HA.readonly True, HA.value (Url.toString gameUrl) ] []
                        , button [ HA.class "menu-item", onClick RequestedMainMenu ] [ text "Cancel" ]
                        ]

                Nothing ->
                    div [ HA.class "buttons" ]
                        [ div [ HA.class "menu-item" ]
                            [ p [] [ text "Creating game..." ]
                            ]
                        , button [ HA.class "menu-item", onClick RequestedMainMenu ] [ text "Cancel" ]
                        ]

        menu =
            div [ HA.id "menu" ] [ titleDiv, mainDiv ]

        containerClass =
            "fade-in"

        menuContainer =
            div [ HA.id "menu-container", HA.class containerClass ] [ menu ]
    in
    menuContainer


viewChooseDifficultyMenu : Html Msg
viewChooseDifficultyMenu =
    let
        title =
            "Choose difficulty:"

        titleDiv =
            div [ HA.class "menutitle" ] [ text title ]

        options =
            div [ HA.class "buttons" ]
                [ button [ HA.class "menu-item", onClick (ChoseDifficulty AI.Easy) ] [ text "Easy" ]
                , button [ HA.class "menu-item", onClick (ChoseDifficulty AI.Normal) ] [ text "Normal" ]
                , button [ HA.class "menu-item", onClick (ChoseDifficulty AI.Hard) ] [ text "Hard" ]
                ]

        menu =
            div [ HA.id "menu" ] [ titleDiv, options ]

        containerClass =
            "fade-in"

        menuContainer =
            div [ HA.id "menu-container", HA.class containerClass ] [ menu ]
    in
    menuContainer


viewMainMenu : Maybe Winner -> Html Msg
viewMainMenu maybeWinner =
    let
        title =
            case maybeWinner of
                Nothing ->
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
                [ button [ HA.class "menu-item", onClick (ChoseMainMenuOption WatchTutorial) ] [ text "How to play" ]
                , button [ HA.class "menu-item", onClick (ChoseMainMenuOption (Play GameMode.OnePlayerVsAI)) ] [ text "1 Player vs AI" ]
                , button [ HA.class "menu-item", onClick (ChoseMainMenuOption (Play GameMode.TwoPlayersLocal)) ] [ text "2 Players (local)" ]
                , button [ HA.class "menu-item", onClick (ChoseMainMenuOption (Play GameMode.TwoPlayersRemote)) ] [ text "2 Players (remote)" ]
                ]

        menu =
            div [ HA.id "menu" ] [ titleDiv, options ]

        containerClass =
            if maybeWinner == Nothing then
                "fade-in"

            else
                "fade-in delay"

        menuContainer =
            div [ HA.id "menu-container", HA.class containerClass ] [ menu ]
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

        cursorStyle =
            case gameSettings of
                LocalVsAI (WaitingForAI _) ->
                    "wait"

                Remote2Players gameId player InProgress ->
                    if player == gameState.currentPlayer then
                        "auto"

                    else
                        "wait"

                _ ->
                    "auto"

        msgType =
            case gameSettings of
                LocalVsAI _ ->
                    PerformedMove Player.X

                _ ->
                    PerformedMove gameState.currentPlayer

        svgView =
            UltimateTicTacToe.svgView msgType gameState
                |> SvgUtils.scale scale
    in
    svg 
        [ SA.viewBox ("0 0 " ++ size ++ " " ++ size)
        , SA.width (size ++ "px")
        , HA.style "cursor" cursorStyle 
        ] [ svgView ]


css : String -> Html a
css path =
    node "link" [ HA.rel "stylesheet", HA.href path ] []
