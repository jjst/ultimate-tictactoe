import UltimateTicTacToe
import TicTacToeBase
import SvgUtils
import Tutorial

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
    , tutorial : Tutorial.Model
    , windowSize : Window.Size
    }

init : (Model, Cmd Msg)
init =
    let
        (tutorial, tutorialMsg) = Tutorial.init
        model =
            { ticTacToe = UltimateTicTacToe.init
            , tutorial = tutorial
            , windowSize = { width = 800, height = 600 }
            }
    in
       model ! [ Cmd.map TutorialMessage tutorialMsg, getWindowSize ]


-- UPDATE

type Msg
    = TicTacToeMessage UltimateTicTacToe.Msg
    | NewWindowSize Window.Size
    | SizeUpdateFailure String -- any chance of this failing? look up doc
    | TutorialMessage Tutorial.Msg

update : Msg -> Model -> (Model, Cmd Msg)
update msg ({ticTacToe, tutorial, windowSize} as model) =
    case msg of
        TicTacToeMessage msg ->
            { model | ticTacToe = (UltimateTicTacToe.update msg ticTacToe) } ! []
        NewWindowSize size ->
            { model | windowSize = size } ! []
        TutorialMessage msg ->
            let
                (newTutorial, cmd) = Tutorial.update msg tutorial
            in
                { model | tutorial = newTutorial } ! [ Cmd.map TutorialMessage cmd ]
        SizeUpdateFailure _ -> model ! []


getWindowSize : Cmd Msg
getWindowSize = Task.perform SizeUpdateFailure NewWindowSize Window.size


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Window.resizes NewWindowSize
    , Sub.map TutorialMessage (Tutorial.subscriptions model.tutorial)
    ]


-- VIEW

view : Model -> Html Msg
view ({ticTacToe, tutorial, windowSize} as model) =
    let
        baseBoardSize = TicTacToeBase.boardSize |> toFloat
        minSize = ((Basics.min windowSize.width windowSize.height) |> toFloat) - 5
        scale = minSize / baseBoardSize
        size = (toString minSize)
        svgView =
            UltimateTicTacToe.svgView ticTacToe
            |> SvgUtils.scale scale
            |> App.map TicTacToeMessage
        tutorialView =
            Tutorial.view tutorial
            |> App.map TutorialMessage
        mainDivStyle =
          Html.Attributes.style
            [ ("margin", "auto")
            , ("position", "relative")
            , ("width", size ++ "px")
            ]
        tutorialStyle =
          Html.Attributes.style
            [ ("z-index", "1")
            , ("left", "5%")
            , ("top", "5%")
            , ("position", "absolute")
            , ("width", "90%")
            , ("font-family", "'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif")
            ]
    in
        div [ mainDivStyle ]
          [ svg [ viewBox ("0 0 " ++ size ++ " " ++ size), width (size ++ "px") ] [ svgView ]
          , div [ tutorialStyle ] [ tutorialView ]
          ]
