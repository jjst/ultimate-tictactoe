module GameServer exposing (..)

import Result
import Http
import Random
import GameId
import Debug
import Task

type GeneratedGameId = GeneratedGameId GameId.GameId

getGameId (GeneratedGameId g) = g

generateGameId : (GameId.GameId -> msg) -> Cmd msg
generateGameId f = 
    Random.generate GeneratedGameId GameId.random
        |> Cmd.map (\g -> f (getGameId g))

createGame : (CreateGameResult -> msg) -> String -> GameId.GameId -> Cmd msg
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

joinGame : (JoinGameResult -> msg) -> String -> GameId.GameId -> Cmd msg
joinGame f serverUrl gameId =
    Task.succeed NotSupportedYet |> Task.perform Problem |> Cmd.map f

parseResponse : GameId.GameId -> Result Http.Error () -> CreateGameResult
parseResponse gameId result =
    case result of
       Ok () -> Success
       Err (Http.BadStatus 409) -> Problem AlreadyExistsError
       Err other -> UnexpectedError other

type GameServerResponse p
    = Success
    | Problem p
    | UnexpectedError Http.Error

type AlreadyExistsError = AlreadyExistsError

type alias CreateGameResult = GameServerResponse AlreadyExistsError

type JoinGameProblem 
    = GameFullError
    | NotSupportedYet
    | GameDoesNotExist

type alias JoinGameResult = GameServerResponse JoinGameProblem
