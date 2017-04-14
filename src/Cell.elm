module Cell exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes as SA
import Svg.Attributes exposing (..)
import String
import Player exposing (..)


type alias Cell = Maybe Player


fromString : String -> Result String Cell
fromString s =
    case (String.toLower s) of
        "o" ->
            Ok (Just Player.O)

        "x" ->
            Ok (Just Player.X)

        "_" ->
            Ok (Nothing)

        other ->
            Err <| "Invalid character: " ++ other


svgView : Cell -> msg -> Svg msg
svgView cell message =
    let
        markDrawing =
            case cell of
                Nothing ->
                    []

                Just X ->
                    [ drawCross ]

                Just O ->
                    [ drawCircle ]
    in
        g []
            ([ rect [ x "0", y "0", width "100", height "100", fillOpacity "0.0", onClick message ] [] ]
                ++ markDrawing
            )

drawCircle : Svg a
drawCircle =
    circle [ cx "50", cy "50", r "40", fillOpacity "0.0", SA.style "stroke:red;stroke-width:6" ] []


drawCross : Svg a
drawCross =
    g [ SA.style "stroke:blue;stroke-width:6" ]
        [ line [ x1 "10", y1 "10", x2 "90", y2 "90" ] []
        , line [ x1 "10", y1 "90", x2 "90", y2 "10" ] []
        ]
