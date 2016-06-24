module Tuple3Test exposing (..)

import ElmTest exposing (runSuite)
import ElmTestBDDStyle exposing (..)

import Tuple3 exposing (..)

fruits = ("apple", "banana", "strawberry")

tests : Test
tests =
  describe "A Tuple3"
    [ it "enumerates indices with range"
        <| expect range toBe (I1, I2, I3)
    , it "returns the first item when calling get with I1"
        <| expect (get fruits I1) toBe "apple"
    , it "returns the second item when calling get with I2"
        <| expect (get fruits I2) toBe "banana"
    , it "returns the third item when calling get with I3"
        <| expect (get fruits I3) toBe "strawberry"
    , it "converts indices to integers"
        <| expect (map toInt range) toBe (0, 1, 2)
    , it "converts indices to floats"
        <| expect (map Tuple3.toFloat range) toBe (0.0, 1.0, 2.0)
    , it "allows item access with !!"
        <| expect (fruits !! I1) toBe "apple"
    , it "returns the first item"
        <| expect (first fruits) toBe "apple"
    , it "returns the second item"
        <| expect (second fruits) toBe "banana"
    , it "returns the third item"
        <| expect (third fruits) toBe "strawberry"
    , it "returns the last item"
        <| expect (last fruits) toBe "strawberry"
    , it "maps a function over each element of the tuple"
        <| expect (map (\x -> x*2) (1, 2, 3)) toBe (2, 4, 6)
    , it "maps a function taking an index  over each element of the tuple"
        <| expect (indexedMap (\i x -> (i,x*2)) (1, 2, 3)) toBe ((I1, 2), (I2, 4), (I3, 6))
    , it "fills a tuple with the given value"
        <| expect (fill 0) toBe (0, 0, 0)
    , it "converts tuples to lists"
        <| expect (toList (1, 2, 3)) toBe [1, 2, 3]
    , it "converts lists of the right size to tuples"
        <| expect (fromList [1, 2, 3]) toBe (Just (1, 2, 3))
    , it "fails to convert lists too short"
        <| expect (fromList [1, 2]) toBe Nothing
    , it "fails to convert lists too long"
        <| expect (fromList [1, 2, 3, 4]) toBe Nothing
    ]


main : Program Never
main =
  runSuite tests
