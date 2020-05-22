module Player exposing (Draw(..), Either(..), Player(..), Winner, opponent, toString)


type Player
    = X
    | O


type Either a b
    = Left a
    | Right b


opponent : Player -> Player
opponent player =
    case player of
        X ->
            O

        O ->
            X

toString : Player -> String
toString player =
    case player of
        X -> "X"
        O -> "O"

type Draw
    = Draw


type alias Winner =
    Either Player Draw
