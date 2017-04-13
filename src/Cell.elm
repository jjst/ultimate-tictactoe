module Cell exposing (..)

import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes as SA
import Svg.Attributes exposing (..)
import String
import Player exposing (..)


-- MODEL


type alias Model =
    { mark : Maybe Player
    , currentPlayer : Player
    }


init : Model
init =
    { mark = Nothing, currentPlayer = X }


fromString : Player -> String -> Result String Model
fromString p s =
    let
        mark =
            case (String.toLower s) of
                "o" ->
                    Ok (Just Player.O)

                "x" ->
                    Ok (Just Player.X)

                "_" ->
                    Ok (Nothing)

                other ->
                    Err <| "Invalid character: " ++ other
    in
        Result.map (\m -> { mark = m, currentPlayer = p }) mark



-- UPDATE


type Msg
    = PlaceMark


update : Msg -> Model -> Model
update msg ({ mark, currentPlayer } as model) =
    case msg of
        PlaceMark ->
            case mark of
                Just _ ->
                    model

                Nothing ->
                    Model (Just currentPlayer) (opponent currentPlayer)



-- VIEW


svgView : Model -> Svg Msg
svgView { mark, currentPlayer } =
    let
        markDrawing =
            case mark of
                Nothing ->
                    []

                Just X ->
                    [ drawCross ]

                Just O ->
                    [ drawCircle ]
    in
        g []
            ([ rect [ x "0", y "0", width "100", height "100", fillOpacity "0.0", onClick PlaceMark ] []
             ]
                ++ markDrawing
            )


view : Model -> Html Msg
view model =
    svg [ viewBox "0 0 100 100", width "300px", transform "scale(0.3)" ]
        [ (svgView model) ]


drawCircle : Svg a
drawCircle =
    circle [ cx "50", cy "50", r "40", fillOpacity "0.0", SA.style "stroke:red;stroke-width:6" ] []


drawCross : Svg a
drawCross =
    g [ SA.style "stroke:blue;stroke-width:6" ]
        [ line [ x1 "10", y1 "10", x2 "90", y2 "90" ] []
        , line [ x1 "10", y1 "90", x2 "90", y2 "10" ] []
        ]
