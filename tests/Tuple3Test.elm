module Tuple3Test exposing (tests)

import Test exposing (..)
import Expect exposing (..)
import ElmTestBDDStyle exposing (..)
import Tuple3 exposing (..)


fruits =
    ( "apple", "banana", "strawberry" )


tests : Test
tests =
    describe "A Tuple3"
        [ it "enumerates indices with range" <|
            expect range to equal ( I1, I2, I3 )
        , it "returns the first item when calling get with I1" <|
            expect (get fruits I1) to equal "apple"
        , it "returns the second item when calling get with I2" <|
            expect (get fruits I2) to equal "banana"
        , it "returns the third item when calling get with I3" <|
            expect (get fruits I3) to equal "strawberry"
        , it "converts indices to integers" <|
            expect (map toInt range) to equal ( 0, 1, 2 )
        , it "converts indices to floats" <|
            expect (map Tuple3.toFloat range) to equal ( 0.0, 1.0, 2.0 )
        , it "allows item access with !!" <|
            expect (fruits !! I1) to equal "apple"
        , it "returns the first item" <|
            expect (first fruits) to equal "apple"
        , it "returns the second item" <|
            expect (second fruits) to equal "banana"
        , it "returns the third item" <|
            expect (third fruits) to equal "strawberry"
        , it "returns the last item" <|
            expect (last fruits) to equal "strawberry"
        , it "maps a function over each element of the tuple" <|
            expect (map (\x -> x * 2) ( 1, 2, 3 )) to equal ( 2, 4, 6 )
        , it "maps a function taking an index  over each element of the tuple" <|
            expect (indexedMap (\i x -> ( i, x * 2 )) ( 1, 2, 3 )) to equal ( ( I1, 2 ), ( I2, 4 ), ( I3, 6 ) )
        , it "fills a tuple with the given value" <|
            expect (fill 0) to equal ( 0, 0, 0 )
        , it "converts tuples to lists" <|
            expect (toList ( 1, 2, 3 )) to equal [ 1, 2, 3 ]
        , it "converts lists of the right size to tuples" <|
            expect (fromList [ 1, 2, 3 ]) to equal (Just ( 1, 2, 3 ))
        , it "fails to convert lists too short" <|
            expect (fromList [ 1, 2 ]) to equal Nothing
        , it "fails to convert lists too long" <|
            expect (fromList [ 1, 2, 3, 4 ]) to equal Nothing
        ]
