module SvgUtils exposing (applyTransform, scale, translate)

import String
import Svg exposing (..)
import Svg.Attributes exposing (..)


applyTransform : String -> Svg a -> Svg a
applyTransform eltTransform element =
    g [ transform eltTransform ]
        [ element ]


scale : Float -> Svg a -> Svg a
scale x elt =
    let
        s =
            String.fromFloat x

        transform =
            "scale(" ++ s ++ "," ++ s ++ ")"
    in
    applyTransform transform elt


translate : Float -> Float -> Svg a -> Svg a
translate x y elt =
    let
        transform =
            "translate(" ++ String.fromFloat x ++ "," ++ String.fromFloat y ++ ")"
    in
    applyTransform transform elt
