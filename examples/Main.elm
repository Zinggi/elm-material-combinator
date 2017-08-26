module Main exposing (main)

import Html exposing (text)
import Html.Attributes
import WebGL
import ShaderStudio exposing (..)
import MaterialCombinator exposing (..)


{-| this is how the library might be used
-}
example =
    customMaterial
        { position =
            glMulVectorMat4 (mat4 .modelViewProjectionMatrix) (glVec3to4 position)
        , fragColor = glVec3to4 normal
        }


example2 =
    customMaterial
        { position =
            glMulVectorMat4 (mat4 .modelViewProjectionMatrix) (glVec3to4 position)
        , fragColor =
            glVec3to4 <|
                (glNormalize
                    (glMulVectorMat3 (glExtract3by3 (mat4 .modelMatrix)) normal)
                )
        }


main =
    let
        ( vert, frag ) =
            extractShaderCode example2
    in
        ShaderStudio.program
            { vertexShader = WebGL.unsafeShader vert
            , fragmentShader = WebGL.unsafeShader frag
            }



--Html.text (toString <| Native.Reflection.getAccessorName .fuckHitler)
