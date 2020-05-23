module LocalDev exposing (..)

import Browser
import Browser.Navigation as Nav
import Html
import Main exposing (Model, Msg(..), subscriptions, update, view)
import Url exposing (Url)


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = ChangedUrl
        , onUrlRequest = ClickedLink
        }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init =
    \_ -> Main.init { remotePlayServerUrl = "https://ultimate-tictactoe-server.osc-fr1.scalingo.io" }
