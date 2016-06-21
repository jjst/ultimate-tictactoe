module Player exposing (..)


type Player
    = X
    | O

opponent : Player -> Player
opponent player =
    case player of
        X -> O
        O -> X
