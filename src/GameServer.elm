port module GameServer exposing (..)

import Board
import Debug
import GameId
import Http
import Parser exposing ((|.), (|=))
import Player exposing (Player)
import Random
import Result
import Task
import Tuple3
import UltimateTicTacToe
import Url



-- PORTS


port joinRemoteGame : String -> Cmd msg


port sendGameMessage : String -> Cmd msg


port gameMessageReceiver : (String -> msg) -> Sub msg



-- SUBSCRIPTIONS


onRemoteGameEvent : GameId.GameId -> (RemoteGameEvent -> msg) -> Sub msg
onRemoteGameEvent gameId f =
    gameMessageReceiver decodeGameMessage |> Sub.map f



-- MODELS


type RemoteGameEvent
    = PlayerMove Player UltimateTicTacToe.Move
    | PlayerJoined Player
    | PlayerLeft Player
    | GameStarted
    | KeepAlive
    | OtherEvent
    | Error String


type GeneratedGameId
    = GeneratedGameId GameId.GameId


type GameServerResponse p
    = Success
    | Problem p
    | UnexpectedError Http.Error


type AlreadyExistsError
    = AlreadyExistsError


type alias CreateGameResult =
    GameServerResponse AlreadyExistsError


type JoinGameProblem
    = GameFullError
    | NotSupportedYet
    | GameDoesNotExist


type alias JoinGameResult =
    GameServerResponse JoinGameProblem


type alias ServerUrl =
    String



-- OTHER STUFF


getGameId (GeneratedGameId g) =
    g


generateGameId : (GameId.GameId -> msg) -> Cmd msg
generateGameId f =
    Random.generate GeneratedGameId GameId.random
        |> Cmd.map (\g -> f (getGameId g))


createGame : (CreateGameResult -> msg) -> ServerUrl -> GameId.GameId -> Cmd msg
createGame f serverUrl gameId =
    Http.request
        { method = "PUT"
        , headers = []
        , url = serverUrl ++ "/games/" ++ gameId
        , body = Http.emptyBody
        , expect = Http.expectWhatever (parseResponse gameId)
        , timeout = Nothing
        , tracker = Nothing
        }
        |> Cmd.map f


parseResponse : GameId.GameId -> Result Http.Error () -> CreateGameResult
parseResponse gameId result =
    case result of
        Ok () ->
            Success

        Err (Http.BadStatus 409) ->
            Problem AlreadyExistsError

        Err other ->
            UnexpectedError other


joinGame : (JoinGameResult -> msg) -> ServerUrl -> GameId.GameId -> Player -> Cmd msg
joinGame f serverUrl gameId player =
    let
        p =
            case player of
                Player.X ->
                    "x"

                Player.O ->
                    "o"
    in
    -- A bit hackish but we're replacing https with wss since it's a websocket connection
    joinRemoteGame (String.replace "https" "wss" (serverUrl ++ "/games/" ++ gameId ++ "/ws/" ++ p))


playMove : ServerUrl -> GameId.GameId -> Player -> UltimateTicTacToe.Move -> Cmd msg
playMove serverUrl gameId player move =
    sendGameMessage ("M " ++ encodePlayer player ++ " " ++ encodeMove move)



-- GAME MESSAGE ENCODING


encodeTuple3Index : Tuple3.Index -> String
encodeTuple3Index idx =
    case idx of
        Tuple3.I1 ->
            "1"

        Tuple3.I2 ->
            "2"

        Tuple3.I3 ->
            "3"


encodeCoords : Board.Coords -> String
encodeCoords ( x, y ) =
    encodeTuple3Index x ++ " " ++ encodeTuple3Index y


encodeMove : UltimateTicTacToe.Move -> String
encodeMove { boardCoords, cellCoords } =
    encodeCoords boardCoords ++ " " ++ encodeCoords cellCoords


encodePlayer : Player.Player -> String
encodePlayer =
    Player.toString



-- GAME MESSAGE DECODING


playerParser : Parser.Parser Player
playerParser =
    Parser.oneOf
        [ Parser.succeed Player.X
            |. Parser.keyword "X"
        , Parser.succeed Player.O
            |. Parser.keyword "O"
        ]


playerJoinedParser : Parser.Parser RemoteGameEvent
playerJoinedParser =
    Parser.succeed PlayerJoined
        |. Parser.keyword "J"
        |. Parser.spaces
        |= playerParser


playerLeftParser : Parser.Parser RemoteGameEvent
playerLeftParser =
    Parser.succeed PlayerLeft
        |. Parser.keyword "L"
        |. Parser.spaces
        |= playerParser


gameStartedParser : Parser.Parser RemoteGameEvent
gameStartedParser =
    Parser.succeed GameStarted
        |. Parser.spaces
        |. Parser.keyword "S"
        |. Parser.spaces


idxParser : Parser.Parser Tuple3.Index
idxParser =
    Parser.oneOf
        [ Parser.succeed Tuple3.I1
            |. Parser.keyword "1"
        , Parser.succeed Tuple3.I2
            |. Parser.keyword "2"
        , Parser.succeed Tuple3.I3
            |. Parser.keyword "3"
        ]


coordsParser : Parser.Parser Board.Coords
coordsParser =
    Parser.succeed Tuple.pair
        |= idxParser
        |. Parser.spaces
        |= idxParser


moveParser : Parser.Parser UltimateTicTacToe.Move
moveParser =
    Parser.succeed UltimateTicTacToe.Move
        |= coordsParser
        |. Parser.spaces
        |= coordsParser


playerMoveParser : Parser.Parser RemoteGameEvent
playerMoveParser =
    Parser.succeed PlayerMove
        |. Parser.symbol "M"
        |. Parser.spaces
        |= playerParser
        |. Parser.spaces
        |= moveParser


keepAliveParser : Parser.Parser RemoteGameEvent
keepAliveParser =
    Parser.succeed KeepAlive
        |. Parser.spaces
        |. Parser.keyword "K"
        |. Parser.spaces


errorParser : Parser.Parser RemoteGameEvent
errorParser =
    Parser.succeed Error
        |. Parser.keyword "E"
        |. Parser.spaces
        |= errorMsgParser


errorMsgParser : Parser.Parser String
errorMsgParser =
    Parser.getChompedString <|
        Parser.succeed ()
            |. Parser.chompWhile (\_ -> True)


messageParser : Parser.Parser RemoteGameEvent
messageParser =
    Parser.oneOf
        [ playerJoinedParser
        , playerLeftParser
        , gameStartedParser
        , playerMoveParser
        , keepAliveParser
        , errorParser
        ]


decodeGameMessage : String -> RemoteGameEvent
decodeGameMessage message =
    case Parser.run messageParser message of
        Ok event ->
            event

        Err _ ->
            OtherEvent
