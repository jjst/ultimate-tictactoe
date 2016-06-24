module Tuple3Test exposing (..)

import ElmTest exposing (..)

import Tuple3 exposing (..)

fruits = ("apple", "banana", "strawberry")

tests : Test
tests =
  suite
    "Tuple3"
    [ test "range" (assertEqual (I1, I2, I3) range)
    , test "get I1" (assertEqual "apple" (get fruits I1))
    , test "get I2" (assertEqual "banana" (get fruits I2))
    , test "get I3" (assertEqual "strawberry" (get fruits I3))
    , test "toInt I1" (assertEqual 0 (toInt I1))
    , test "toInt I2" (assertEqual 1 (toInt I2))
    , test "toInt I3" (assertEqual 2 (toInt I3))
    , test "toFloat I1" (assertEqual 0.0 (Tuple3.toFloat I1))
    , test "!! operator" (assertEqual "apple" (fruits !! I1))
    , test "first" (assertEqual "apple" (first fruits))
    , test "second" (assertEqual "banana" (second fruits))
    , test "third" (assertEqual "strawberry" (third fruits))
    , test "last" (assertEqual "strawberry" (last fruits))
    , test "map" (assertEqual (2, 4, 6) (map (\x -> x*2) (1, 2, 3)))
    , test "indexedMap" (assertEqual ((I1, 2), (I2, 4), (I3, 6)) (indexedMap (\i x -> (i,x*2)) (1, 2, 3)))
    , test "fill" (assertEqual (0, 0, 0) (fill 0))
    , test "toList" (assertEqual [1, 2, 3] (toList (1, 2, 3)))
    , test "fromList right size" (assertEqual (Just (1, 2, 3)) (fromList [1, 2, 3]))
    , test "fromList too short" (assertEqual Nothing (fromList [1, 2]))
    , test "fromList too long" (assertEqual Nothing (fromList [1, 2, 3, 4]))
    ]


main : Program Never
main =
  runSuite tests
