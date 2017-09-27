module MaterialCombinator.Types exposing (..)

import Dict exposing (Dict)


{-| This type just bundles a vertex/fragment shader pair
-}
type Material attributes uniforms
    = Material { frag : String, vert : String }


{-| This type represents a wire in UE4's material editor
-}
type Unit uniforms attributes type_
    = Unit { source : String, uniforms : Dict String String, attributes : Dict String String }
