module Window exposing (show)

import Html exposing (Html, Attribute)
import Html.Attributes as HA


show : String -> List (Attribute msg) -> Html msg -> Html msg
show title attrs contents =
    let
        titleDiv =
            Html.div [ HA.class "windowtitle" ] [ Html.text title ]

        contentsDiv =
            Html.div [ HA.class "windowcontent" ]
                [ contents ]

        window =
            Html.div [ HA.id "window" ] [ titleDiv, contents ]

        menuContainer =
            Html.div (attrs ++ [ HA.id "window-container" ]) [ window ]
    in
    menuContainer
