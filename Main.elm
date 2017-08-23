module Main exposing (main)

import Html exposing (text)
import Html.Attributes
import Native.Reflection
import MaterialPower exposing (..)


main =
    Html.div []
        [ Html.textarea [] [ text (getVert example2) ]
        , Html.textarea [] [ text (getFrag example2) ]
        ]



--Html.text (toString <| Native.Reflection.getAccessorName .fuckHitler)
