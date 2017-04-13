module Main exposing (..)

import UltimateTicTacToeWithAI
import UltimateTicTacToe
import TicTacToeBase
import SvgUtils
import Menu
import Task
import Window
import Html.Attributes as HA
import Html
import Html exposing (Html, div, node)
import Svg exposing (svg)
import Svg.Attributes as SA


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { ticTacToe : UltimateTicTacToe.Model
    , menu : Menu.Model
    , windowSize : Window.Size
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { ticTacToe = UltimateTicTacToe.init
            , menu = Menu.init
            , windowSize = { width = 800, height = 600 }
            }
    in
        model ! [ getWindowSize ]



-- UPDATE


type Msg
    = TicTacToeMessage UltimateTicTacToe.Msg
    | NewWindowSize Window.Size
    | SizeUpdateFailure String -- any chance of this failing? look up doc
    | MenuMessage Menu.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ ticTacToe, menu, windowSize } as model) =
    let
        newModel =
            case msg of
                TicTacToeMessage msg ->
                    let
                        update =
                            case menu of
                                Menu.OnePlayerVsAI ->
                                    UltimateTicTacToeWithAI.update

                                Menu.TwoPlayers ->
                                    UltimateTicTacToe.update

                                _ ->
                                    UltimateTicTacToe.update
                    in
                        { model | ticTacToe = (update msg ticTacToe) }

                NewWindowSize size ->
                    { model | windowSize = size }

                MenuMessage msg ->
                    { model | menu = Menu.update msg menu }

                SizeUpdateFailure _ ->
                    model
    in
        newModel ! []


getWindowSize : Cmd Msg
getWindowSize =
    Task.perform SizeUpdateFailure NewWindowSize Window.size



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes NewWindowSize ]



-- VIEW


view : Model -> Html Msg
view ({ ticTacToe, menu, windowSize } as model) =
    let
        baseBoardSize =
            TicTacToeBase.boardSize |> toFloat

        minSize =
            ((Basics.min windowSize.width windowSize.height) |> toFloat) - 5

        scale =
            minSize / baseBoardSize

        size =
            (toString minSize)

        svgView =
            UltimateTicTacToe.svgView ticTacToe
                |> SvgUtils.scale scale
                |> Html.map TicTacToeMessage

        menuView =
            Menu.view menu
                |> Html.map MenuMessage

        mainDivStyle =
            HA.style
                [ ( "margin", "auto" )
                , ( "position", "relative" )
                , ( "width", size ++ "px" )
                ]

        menuStyle =
            HA.style
                [ ( "z-index", "1" )
                , ( "left", "5%" )
                , ( "top", "5%" )
                , ( "position", "absolute" )
                , ( "width", "90%" )
                , ( "font-family", "'Source Sans Pro', 'Trebuchet MS', 'Lucida Grande', 'Bitstream Vera Sans', 'Helvetica Neue', sans-serif" )
                ]
    in
        div [ mainDivStyle ]
            [ css "style.css"
            , svg [ SA.viewBox ("0 0 " ++ size ++ " " ++ size), SA.width (size ++ "px") ] [ svgView ]
            , div [ menuStyle ] [ menuView ]
            ]


css : String -> Html a
css path =
    node "link" [ HA.rel "stylesheet", HA.href path ] []
