module BlinnPhong exposing (..)

import WebGL
import ShaderStudio exposing (..)
import MaterialCombinator exposing (..)
import MaterialCombinator.Operations exposing (..)
import Html exposing (Html, div, textarea)
import Html.Attributes as Attr


material =
    blinnPhongMaterial
        { diffuse = xyz (sampleTexture (texture .textureDiff))
        , normal = xyz (sampleTexture (texture .textureNorm))
        }


debugDisplayCode : Material a u -> Html Never
debugDisplayCode m =
    let
        ( vert, frag ) =
            extractShaderCode m
    in
        div []
            [ div [] [ textarea [ Attr.value vert ] [] ]
            , div [] [ textarea [ Attr.value frag ] [] ]
            ]


main =
    -- debugDisplayCode material
    let
        ( vert, frag ) =
            extractShaderCode material
                |> Debug.log "code:\n\n"
    in
        ShaderStudio.program
            { vertexShader = WebGL.unsafeShader vert
            , fragmentShader = WebGL.unsafeShader frag
            , defaultTexture = "textures/elmBrickDiff.png"
            , textures = [ "textures/elmBrickNorm.png" ]
            }
