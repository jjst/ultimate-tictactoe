module Tuple3 exposing (..)

import Basics as B


type alias Tuple3 a =
    ( a, a, a )


type Index
    = I1
    | I2
    | I3


range : Tuple3 Index
range =
    ( I1, I2, I3 )


get : Tuple3 a -> Index -> a
get ( first, second, third ) idx =
    case idx of
        I1 ->
            first

        I2 ->
            second

        I3 ->
            third


toFloat : Index -> Float
toFloat index =
    index |> toInt |> B.toFloat


toInt : Index -> Int
toInt index =
    case index of
        I1 ->
            0

        I2 ->
            1

        I3 ->
            2


(!!) : Tuple3 a -> Index -> a
(!!) tuple idx =
    get tuple idx
infixl 9 !!


first : Tuple3 a -> a
first ( fst, _, _ ) =
    fst


second : Tuple3 a -> a
second ( _, scnd, _ ) =
    scnd


third : Tuple3 a -> a
third ( _, _, thrd ) =
    thrd


last : Tuple3 a -> a
last =
    third


map : (a -> b) -> Tuple3 a -> Tuple3 b
map f ( a1, a2, a3 ) =
    ( f a1, f a2, f a3 )


indexedMap : (Index -> a -> b) -> Tuple3 a -> Tuple3 b
indexedMap f ( a1, a2, a3 ) =
    ( f I1 a1, f I2 a2, f I3 a3 )


fill : a -> Tuple3 a
fill item =
    ( item, item, item )


toList : Tuple3 a -> List a
toList ( i1, i2, i3 ) =
    [ i1, i2, i3 ]


fromList : List a -> Maybe (Tuple3 a)
fromList l =
    case l of
        [ a1, a2, a3 ] ->
            Just ( a1, a2, a3 )

        _ ->
            Nothing
