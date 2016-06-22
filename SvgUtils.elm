module SvgUtils exposing (..)

import Svg exposing (..)
import Svg.Attributes exposing (..)

applyTransform : String -> Svg a -> Svg a
applyTransform eltTransform element =
    g [ transform eltTransform ]
      [ element ]

scale : number -> number -> Svg a -> Svg a
scale x y elt = 
    let
        transform = "scale(" ++ toString x ++ "," ++ toString y ++ ")"
    in
        applyTransform transform elt

translate : number -> number -> Svg a -> Svg a
translate x y elt = 
    let
        transform = "translate(" ++ toString x ++ "," ++ toString y ++ ")"
    in
        applyTransform transform elt
