module Tutorial exposing (..)

import Html exposing (Html, button, div, input, node, p, text)
import Html.Attributes as HA
import Window

-- model

type Model = Model

type Msg = Msg

init : Model
init = Model

-- update


update : Msg -> Model -> Model
update msg model =
    model


-- view

view : Model -> Html a
view model =
    let
        contents =
            [ div [ HA.class "menu-item" ]
                [ p [] [ text "Just testing..." ]
                ]
            , button [ HA.class "menu-item" ] [ text "Okay" ]
            ]
    in
    Window.show "Test" [] contents
