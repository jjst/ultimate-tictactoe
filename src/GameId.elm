module GameId exposing (GameId, random)
import Random
import Random.String
import Random.Char

type alias GameId = String

random : Random.Generator GameId
random = Random.String.string 4 Random.Char.english |> Random.map (String.toUpper)
