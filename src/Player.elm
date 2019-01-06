module Player exposing (Draw(..), Either(..), Player(..), Winner, opponent)


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


type Draw
    = Draw


type alias Winner =
    Either Player Draw
