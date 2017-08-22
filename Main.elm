module Main exposing (main)

import Html exposing (text)
import Html.Attributes
import Native.Reflection
import MaterialPower exposing (..)


main =
    Html.div []
        [ Html.textarea [] [ text (getVert example) ]
        , Html.textarea [] [ text (getFrag example) ]
        ]



--Html.text (toString <| Native.Reflection.getAccessorName .fuckHitler)
