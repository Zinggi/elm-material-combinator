module MaterialCombinator exposing (..)

{-|


# General concept

I tried to translate [UE4's material editor](https://docs.unrealengine.com/latest/INT/Engine/Rendering/Materials/Editor/index.html) to an elm API

A wire maps to a Unit.

Boxes map to functions.

The lightning model is a function that takes some Units and produces a Material.

-}

import Math.Vector3 exposing (Vec3, vec3)
import Math.Vector2 exposing (Vec2)
import WebGL exposing (Shader, Texture)


{-| this is how the library might be used
-}
example : Material { uvCoordinates : Vec2, vertexPosition : Vec3 } { color : Vec3, texture : Texture } { vpos : Vec3, vcoord : Vec2 }
example =
    physicallyBasedMaterial
        (\{ texture, color } ->
            { baseColor =
                -- multiply `color` to the input `texture`
                multiply
                    (sampleUV (unit texture))
                    (unit color)
            , roughness =
                -- use the green channel for roughness
                extraxtGreen (unit texture)
            }
        )


{-| This type just bundles a vertex/fragment shader pair
-}
type alias Material attributes uniforms varyings =
    { frag : Shader {} uniforms varyings
    , vert : Shader attributes uniforms varyings
    }


{-| This type represents a wire in UE4's material editor
-}
type Unit type_
    = Unit


{-| We could have different ligning models here.
A lighning model is a general way to describe a common rendering pipeline.
-}
physicallyBasedMaterial :
    (uniforms
     ->
        { baseColor : Unit Vec3
        , roughness : Unit Float
        }
    )
    -> Material { uvCoordinates : Vec2, vertexPosition : Vec3 } uniforms { vpos : Vec3, vcoord : Vec2 }
physicallyBasedMaterial =
    Debug.crash ""


{-| create an input variable
-}
unit : a -> Unit a
unit =
    Debug.crash ""


{-| sample a texture
-}
sampleUV : Unit Texture -> Unit Vec3
sampleUV =
    Debug.crash ""


{-| extract the green texture chanel
-}
extraxtGreen : Unit Texture -> Unit Float
extraxtGreen =
    Debug.crash ""


{-| multiply two vectors
-}
multiply : Unit Vec3 -> Unit Vec3 -> Unit Vec3
multiply =
    Debug.crash ""
