module Main exposing (main)

import Html
import Native.Reflection


main =
    Html.text (toString <| Native.Reflection.getAccessorName .fuckHitler)
