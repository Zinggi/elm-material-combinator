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
import WebGL exposing (Shader, Texture, Mesh)
import Native.Reflection


{-| this is how the library might be used
-}
example : Material { uvCoordinates : Vec2, vertexPosition : Vec3 } { color : Vec3, texture : Texture }
example =
    physicallyBasedMaterial
        { baseColor =
            -- multiply `color` to the input `texture`
            multiply
                (sampleUV (texture .texture))
                (vec3 .color)
        , roughness =
            -- use the green channel for roughness
            extractGreen (texture .texture)
        }


{-| This type just bundles a vertex/fragment shader pair
-}
type Material attributes uniforms
    = Material


{-| This type represents a wire in UE4's material editor
-}
type Unit uniforms type_
    = Unit


render : Material attributes uniforms -> Mesh attributes -> uniforms -> WebGL.Entity
render =
    Debug.crash "todo"


{-| We could have different ligning models here.
A lighning model is a general way to describe a common rendering pipeline.
-}
physicallyBasedMaterial :
    { baseColor : Unit uniforms Vec3
    , roughness : Unit uniforms Float
    }
    -> Material { uvCoordinates : Vec2, vertexPosition : Vec3 } uniforms
physicallyBasedMaterial =
    Debug.crash ""


{-| create an input variable.
This would not be exported to the user!

(For implementation, this signature would require native code, to figure out the name of the property accessed.
without native, the api could be changed to:
uniform : (uniforms -> a) -> String -> Unit uniforms a
however, this way a user would have to remember to always use the same string as the accessor
e.g: `uniform .texture "texture"`. This would make it prone to some errors.

-}
uniform : (uniforms -> a) -> String -> Unit uniforms a
uniform f type_ =
    --Native.Reflection.getAccessorName f
    Debug.crash ""


texture : (uniforms -> Texture) -> Unit uniforms Texture
texture =
    Debug.crash "todo"


vec3 : (uniforms -> Vec3) -> Unit uniforms Vec3
vec3 =
    Debug.crash "todo"


{-| sample a texture
-}
sampleUV : Unit uniforms Texture -> Unit uniforms Vec3
sampleUV =
    Debug.crash ""


{-| extract the green texture chanel
-}
extractGreen : Unit uniforms Texture -> Unit uniforms Float
extractGreen =
    Debug.crash ""


{-| multiply two vectors
-}
multiply : Unit uniforms Vec3 -> Unit uniforms Vec3 -> Unit uniforms Vec3
multiply =
    Debug.crash ""
