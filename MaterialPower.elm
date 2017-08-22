module MaterialPower exposing (..)

{-| -}

import Math.Matrix4 exposing (Mat4)
import Math.Vector4 exposing (Vec4)
import Math.Vector3 exposing (Vec3)
import Math.Vector2 exposing (Vec2)
import WebGL exposing (Shader, Texture, Mesh, entity)
import Dict exposing (Dict)
import Native.Reflection


{-| this is how the library might be used
-}
example =
    customMaterial
        { position =
            glMulVectorMat4 (uniform .mvp "mvp" "mat4")
                (glVec3to4 (attribute .position "position" "vec4"))
        , fragColor = glVec3to4 (attribute .vertexNormal "vertexNormal" "vec3")
        }


render : Material attributes uniforms -> Mesh attributes -> uniforms -> WebGL.Entity
render material =
    entity (WebGL.unsafeShader (getVert material)) (WebGL.unsafeShader (getFrag material))


getVert : Material a u -> String
getVert (Material a) =
    a.vert


getFrag : Material a u -> String
getFrag (Material a) =
    a.frag


{-| This type just bundles a vertex/fragment shader pair
-}
type Material attributes uniforms
    = Material { frag : String, vert : String }


{-| This type represents a wire in UE4's material editor
-}
type Unit uniforms attributes type_
    = Unit { source : String, uniforms : Dict String String, attributes : Dict String String }


{-| create an input variable
-}
uniform : (uniforms -> a) -> String -> String -> Unit uniforms attributes a
uniform f name type_ =
    Unit { source = name, uniforms = Dict.singleton name type_, attributes = Dict.empty }


attribute : (attributes -> a) -> String -> String -> Unit uniforms attributes a
attribute f name type_ =
    Unit { source = name, attributes = Dict.singleton name type_, uniforms = Dict.empty }


customMaterial :
    { position : Unit uniforms attributes Vec4
    , fragColor : Unit uniforms attributes Vec4
    }
    -> Material uniforms attributes
customMaterial { position, fragColor } =
    let
        ( declarations, vertCode, fragCode ) =
            case ( position, fragColor ) of
                ( Unit uv, Unit uf ) ->
                    ( (Dict.toList uv.uniforms
                        |> List.map
                            (\( name, type_ ) ->
                                "uniform " ++ type_ ++ " " ++ name
                            )
                        |> String.join ";\n"
                      )
                        ++ ";\n"
                    , uv.source ++ ";\n"
                    , uf.source
                    )
    in
        Material
            { vert =
                "precision mediump float;\n"
                    ++ declarations
                    ++ "void main() {\n"
                    ++ "gl_Position = "
                    ++ vertCode
                    ++ "}\n"
            , frag =
                "precision mediump float;\n"
                    ++ declarations
                    ++ "void main() {\n"
                    ++ "gl_FragColor = "
                    ++ fragCode
                    ++ "}\n"
            }


glMulVectorMat4 : Unit u a Mat4 -> Unit u a Vec4 -> Unit u a Vec4
glMulVectorMat4 (Unit mat) (Unit vec) =
    mergeUnits mat vec (mat.source ++ " * " ++ vec.source)


mergeUnits u1 u2 source =
    Unit
        { source = source
        , attributes =
            Dict.union u1.attributes u2.attributes
        , uniforms = Dict.union u1.uniforms u2.uniforms
        }


glVec3to4 : Unit u a Vec3 -> Unit u a Vec4
glVec3to4 (Unit vec) =
    Unit { vec | source = "vec4(" ++ vec.source ++ ",1.0)" }
