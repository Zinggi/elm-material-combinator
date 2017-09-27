module Main exposing (main)

import WebGL
import ShaderStudio exposing (..)
import MaterialCombinator exposing (..)
import MaterialCombinator.Operations exposing (..)


{-| this is how the library might be used
-}
example =
    customMaterial
        { position =
            mulVectorMat4 (mat4 .modelViewProjectionMatrix) (vec3to4 position)
        , fragColor = vec3to4 normal
        }


example2 =
    customMaterial
        { position =
            mulVectorMat4 (mat4 .modelViewProjectionMatrix) (vec3to4 position)
        , fragColor =
            sampleTexture (texture .textureDiff)
        }


main =
    let
        ( vert, frag ) =
            extractShaderCode example2
    in
        ShaderStudio.program
            { vertexShader = WebGL.unsafeShader vert
            , fragmentShader = WebGL.unsafeShader frag
            , defaultTexture = "textures/elmBrickDiff.png"
            , textures = [ "textures/elmBrickNorm.png" ]
            }
