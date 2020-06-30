module Cell exposing (Cell, drawCircle, drawCross, fromString, svgView)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Player exposing (..)
import String
import Svg exposing (..)
import Svg.Attributes as SA exposing (..)


type alias Cell =
    Maybe Player


fromString : String -> Result String Cell
fromString s =
    case String.toLower s of
        "o" ->
            Ok (Just Player.O)

        "x" ->
            Ok (Just Player.X)

        "_" ->
            Ok Nothing

        other ->
            Err <| "Invalid character: " ++ other


svgView : Maybe Player -> Cell -> msg -> Svg msg
svgView maybePlayer cell message =
    case cell of
        Nothing ->
            drawEmptyCell message maybePlayer

        Just player ->
            drawFilledCell player


drawEmptyCell : msg -> Maybe Player -> Svg msg
drawEmptyCell message maybeGhost =
    let
        rectAttrs =
            [ x "0", y "0", width "100", height "100", fillOpacity "0.0", onClick message ]

        clickArea =
            rect rectAttrs []
    in
    g [ SA.class "show-on-hover" ] [ drawGhost maybeGhost, clickArea ]



drawGhost maybePlayer =
    case maybePlayer of
        Nothing -> 
            g [] []
        Just X ->
            g [] [ drawCross ]
        Just O ->
            g [] [ drawCircle ]

drawFilledCell : Player -> Svg a
drawFilledCell player =
    let
        mark = case player of
            X -> drawCross
            O -> drawCircle
    in
    g [ SA.class "mark" ] [ mark ]



drawCircle : Svg a
drawCircle =
    circle [ cx "50", cy "50", r "40", fillOpacity "0.0", SA.style "stroke:red;stroke-width:6" ] []


drawCross : Svg a
drawCross =
    g [ SA.style "stroke:blue;stroke-width:6" ]
        [ line [ x1 "10", y1 "10", x2 "90", y2 "90" ] []
        , line [ x1 "10", y1 "90", x2 "90", y2 "10" ] []
        ]
