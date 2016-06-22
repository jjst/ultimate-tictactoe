module Player exposing (..)


type Player
    = X
    | O

opponent : Player -> Player
opponent player =
    case player of
        X -> O
        O -> X

type Winner = Cross | Circle | Draw

fromPlayer : Player -> Winner
fromPlayer player = case player of
  O -> Circle
  X -> Cross
