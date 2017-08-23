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
                (glVec3to4 (attribute .position "position" "vec3"))
        , fragColor = glVec3to4 (attribute .vertexNormal "vertexNormal" "vec3")
        }


example2 =
    customMaterial
        { position =
            glMulVectorMat4
                (uniform .projectionMatrix "projectionMatrix" "mat4")
                (glMulVectorMat4 (uniform .modelViewMatrix "modelViewMatrix" "mat4")
                    (glVec3to4 (attribute .position "position" "vec3"))
                )
        , fragColor =
            glNormalize
                (glMulVectorMat3 (uniform .normalMatrix "normalMatrix" "mat3")
                    (attribute .normal "normal" "vec3")
                )
                |> glVec3to4
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


getUniforms : Unit a u t -> String
getUniforms (Unit u) =
    dictToDeclaration "uniform" "" u.uniforms


getAttributes : Unit a1 u1 t1 -> Unit a1 u1 t1 -> String
getAttributes (Unit uv) (Unit uf) =
    Dict.union uv.attributes uf.attributes
        |> dictToDeclaration "attribute" ""


dictToDeclaration : String -> String -> Dict String String -> String
dictToDeclaration mod nameMod d =
    appendIfNotEmpty ";\n"
        (Dict.toList d
            |> List.map
                (\( name, type_ ) ->
                    mod ++ " " ++ type_ ++ " " ++ nameMod ++ name
                )
            |> String.join ";\n"
        )


appendIfNotEmpty appendum s =
    if s == "" then
        ""
    else
        s ++ appendum


{-| TODO: this is an ugly hack because my datastructure for Unit is too primitive.
A better idea would be to create an AST and then a code generator for that
-}
varyingToAttrHack (Unit d) =
    appendIfNotEmpty ";\n"
        (Dict.toList d.attributes
            |> List.map
                (\( name, type_ ) ->
                    type_ ++ " " ++ name ++ " = " ++ "v" ++ name
                )
            |> String.join ";\n"
        )


getSource : Unit a u t -> String
getSource (Unit u) =
    u.source ++ ";\n"


getVaryings : Unit u a t -> ( String, String )
getVaryings (Unit u) =
    let
        declr =
            dictToDeclaration "varying" "v" u.attributes

        assign =
            appendIfNotEmpty ";\n"
                (Dict.toList u.attributes
                    |> List.map
                        (\( name, _ ) ->
                            "v" ++ name ++ " = " ++ name
                        )
                    |> String.join ";\n"
                )
    in
        ( declr, assign )


customMaterial :
    { position : Unit uniforms attributes Vec4
    , fragColor : Unit uniforms attributes Vec4
    }
    -> Material uniforms attributes
customMaterial { position, fragColor } =
    let
        ( varyingDeclarations, varyingAssignments ) =
            getVaryings fragColor
    in
        Material
            { vert =
                "precision mediump float;\n"
                    ++ getUniforms position
                    ++ getAttributes position fragColor
                    ++ varyingDeclarations
                    ++ "void main() {\n"
                    ++ varyingAssignments
                    ++ "gl_Position = "
                    ++ getSource position
                    ++ "}\n"
            , frag =
                "precision mediump float;\n"
                    ++ getUniforms fragColor
                    ++ varyingDeclarations
                    ++ "void main() {\n"
                    ++ varyingToAttrHack fragColor
                    ++ "gl_FragColor = "
                    ++ getSource fragColor
                    ++ "}\n"
            }


glMulVectorMat4 : Unit u a Mat4 -> Unit u a Vec4 -> Unit u a Vec4
glMulVectorMat4 (Unit mat) (Unit vec) =
    mergeUnits mat vec ("(" ++ mat.source ++ " * " ++ vec.source ++ ")")


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


glNormalize : Unit u a Vec3 -> Unit u a Vec3
glNormalize (Unit u) =
    Unit { u | source = "normalize(" ++ u.source ++ ")" }


type Mat3
    = Mat3


glMulVectorMat3 : Unit u a Mat3 -> Unit u a Vec3 -> Unit u a Vec3
glMulVectorMat3 (Unit mat) (Unit vec) =
    mergeUnits mat vec ("(" ++ mat.source ++ " * " ++ vec.source ++ ")")
