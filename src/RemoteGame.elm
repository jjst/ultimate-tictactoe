module RemoteGame exposing (..)

import Random
import GameId

type GeneratedGameId = GeneratedGameId GameId.GameId

getGameId (GeneratedGameId g) = g

createNewGame : (GameId.GameId -> msg) -> Cmd msg
createNewGame f = 
    Random.generate GeneratedGameId GameId.random
        |> Cmd.map (\g -> f (getGameId g))
