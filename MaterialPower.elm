module MaterialPower exposing (..)

{-| -}

import Math.Matrix4 exposing (Mat4)
import Math.Vector4 exposing (Vec4)
import Math.Vector3 exposing (Vec3)
import Math.Vector2 exposing (Vec2)
import WebGL exposing (Shader, Texture, Mesh, entity)
import Dict exposing (Dict)
import Native.Reflection


getAccessorName : (a -> b) -> Result String String
getAccessorName =
    Native.Reflection.getAccessorName


{-| this is how the library might be used
-}
example =
    customMaterial
        { position =
            glMulVectorMat4 (mat4 .mvp)
                (glVec3to4 position)
        , fragColor = glVec3to4 vertexNormal
        }


example2 =
    customMaterial
        { position =
            glMulVectorMat4
                (mat4 .projectionMatrix)
                (glMulVectorMat4 (mat4 .modelViewMatrix)
                    (glVec3to4 position)
                )
        , fragColor =
            glNormalize
                (glMulVectorMat3 (uniform .normalMatrix "mat3") normal)
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


uniform : (uniforms -> a) -> String -> Unit uniforms attributes a
uniform f type_ =
    case getAccessorName f of
        Err e ->
            Debug.crash "You provided a uniform accessor that was not in the form of '.someVariable'\nAborting!"

        Ok name ->
            Unit { source = name, uniforms = Dict.singleton name type_, attributes = Dict.empty }


attribute : (attributes -> a) -> String -> Unit uniforms attributes a
attribute f type_ =
    case getAccessorName f of
        Err e ->
            Debug.crash "You provided a uniform accessor that was not in the form of '.someVariable'\nAborting!"

        Ok name ->
            Unit { source = name, attributes = Dict.singleton name type_, uniforms = Dict.empty }



-- uniform types


texture : (uniforms -> Texture) -> Unit uniforms attributes Texture
texture f =
    uniform f "sampler2D"


vec2 : (uniforms -> Vec2) -> Unit uniforms attributes Vec2
vec2 f =
    uniform f "vec2"


vec3 : (uniforms -> Vec3) -> Unit uniforms attributes Vec3
vec3 f =
    uniform f "vec3"


vec4 : (uniforms -> Vec4) -> Unit uniforms attributes Vec4
vec4 f =
    uniform f "vec4"


mat4 : (uniforms -> Mat4) -> Unit uniforms attributes Mat4
mat4 f =
    uniform f "mat4"


float : (uniforms -> Float) -> Unit uniforms attributes Float
float f =
    uniform f "float"


int : (uniforms -> Int) -> Unit uniforms attributes Int
int f =
    uniform f "int"



-- common attributes


position : Unit uniforms { a | position : Vec3 } Vec3
position =
    attribute .position "vec3"


vertexNormal : Unit uniforms { a | vertexNormal : Vec3 } Vec3
vertexNormal =
    attribute .vertexNormal "vec3"


normal : Unit uniforms { a | normal : Vec3 } Vec3
normal =
    attribute .normal "vec3"



-- attribute types


vec2Attribute : (attributes -> Vec2) -> Unit uniforms attributes Vec2
vec2Attribute f =
    attribute f "vec2"


vec3Attribute : (attributes -> Vec3) -> Unit uniforms attributes Vec3
vec3Attribute f =
    attribute f "vec3"


vec4Attribute : (attributes -> Vec4) -> Unit uniforms attributes Vec4
vec4Attribute f =
    attribute f "vec4"


mat4Attribute : (attributes -> Mat4) -> Unit uniforms attributes Mat4
mat4Attribute f =
    attribute f "mat4"


floatAttribute : (attributes -> Float) -> Unit uniforms attributes Float
floatAttribute f =
    attribute f "float"


intAttribute : (attributes -> Int) -> Unit uniforms attributes Int
intAttribute f =
    attribute f "int"



------------------------------------------
-- Implementation
------------------------------------------


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
