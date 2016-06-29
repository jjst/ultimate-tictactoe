import UltimateTicTacToe
import TicTacToeBase
import SvgUtils

import Task
import Window
import Html.Attributes
import Html exposing (Html, div)
import Html.App as App
import Svg exposing (..)
import Svg.Attributes exposing (..)



main =
    App.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL

type alias Model =
    { ticTacToe : UltimateTicTacToe.Model
    , windowSize : Window.Size
    }

init : (Model, Cmd Msg)
init =
    let
        model =
            { ticTacToe = UltimateTicTacToe.init
            , windowSize = { width = 800, height = 600 }
            }
    in
       (model, getWindowSize)


-- UPDATE

type Msg
    = TicTacToeMessage UltimateTicTacToe.Msg
    | NewWindowSize Window.Size
    | SizeUpdateFailure String -- any chance of this failing? look up doc

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ticTacToe, windowSize} as model) =
    let
        model =
            case msg of
                TicTacToeMessage msg ->
                    { ticTacToe = (UltimateTicTacToe.update msg ticTacToe), windowSize = windowSize }
                NewWindowSize size ->
                    { ticTacToe = ticTacToe, windowSize = size }
                _ -> model
    in
       (model, Cmd.none)


getWindowSize : Cmd Msg
getWindowSize = Task.perform SizeUpdateFailure NewWindowSize Window.size


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Window.resizes NewWindowSize


-- VIEW

view : Model -> Html Msg
view ({ticTacToe, windowSize} as model) =
    let
        baseBoardSize = TicTacToeBase.boardSize |> toFloat
        minSize = (Basics.min windowSize.width windowSize.height) |> toFloat
        scale = minSize / baseBoardSize
        size = (toString minSize)
        divStyle =
          Html.Attributes.style
            [ ("margin", "auto")
            , ("width", size ++ "px")
            ]
        svgView =
            UltimateTicTacToe.svgView ticTacToe
            |> SvgUtils.scale scale
            |> App.map TicTacToeMessage
    in
        div [ divStyle ]
            [
            svg [ viewBox ("0 0 " ++ size ++ " " ++ size), width (size ++ "px") ]
                [ svgView ]
            ]
