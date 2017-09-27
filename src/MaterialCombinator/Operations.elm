module MaterialCombinator.Operations
    exposing
        ( sampleUV
        , sampleTexture
        , uniform
        , attribute
        , texture
        , vec2
        , vec3
        , vec4
        , mat4
        , float
        , int
        , constInt
        , constFloat
        , constVec2
        , constVec3
        , constVec4
        , position
        , vertexNormal
        , normal
        , texCoord
        , xyz
        , vec2Attribute
        , vec3Attribute
        , vec4Attribute
        , intAttribute
        , floatAttribute
        , mat4Attribute
        , mulVectorMat4
        , mulVector3
        , mulVector4
        , vec3to4
        )

import Math.Vector2 as V2 exposing (Vec2)
import Math.Vector3 as V3 exposing (Vec3)
import Math.Vector4 as V4 exposing (Vec4)
import Math.Matrix4 exposing (Mat4)
import WebGL exposing (Texture)
import Dict exposing (Dict)


--

import MaterialCombinator.Types exposing (..)
import Native.Reflection


type Mat3
    = Mat3


getAccessorName : (a -> b) -> Result String String
getAccessorName =
    Native.Reflection.getAccessorName



-- basics


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
            Debug.crash "You provided an attribute accessor that was not in the form of '.someVariable'\nAborting!"

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



-- constants


constInt : Int -> Unit uniforms attributes Int
constInt i =
    Unit { source = toString i, uniforms = Dict.empty, attributes = Dict.empty }


constFloat : Float -> Unit uniforms attributes Float
constFloat i =
    Unit { source = toString i, uniforms = Dict.empty, attributes = Dict.empty }


constVec2 : Vec2 -> Unit uniforms attributes Vec2
constVec2 v =
    let
        ( x, y ) =
            V2.toTuple v
    in
        Unit { source = "vec2(" ++ toString x ++ "," ++ toString y ++ ")", uniforms = Dict.empty, attributes = Dict.empty }


constVec3 : Vec3 -> Unit uniforms attributes Vec3
constVec3 v =
    let
        ( x, y, z ) =
            V3.toTuple v
    in
        Unit
            { source = "vec3(" ++ toString x ++ "," ++ toString y ++ "," ++ toString z ++ ")"
            , uniforms = Dict.empty
            , attributes = Dict.empty
            }


constVec4 : Vec4 -> Unit uniforms attributes Vec4
constVec4 v =
    let
        ( x, y, z, w ) =
            V4.toTuple v
    in
        Unit
            { source = "vec4(" ++ toString x ++ "," ++ toString y ++ "," ++ toString z ++ "," ++ toString w ++ ")"
            , uniforms = Dict.empty
            , attributes = Dict.empty
            }



-- common attributes


position : Unit uniforms { a | position : Vec3 } Vec3
position =
    attribute .position "vec3"


texCoord : Unit uniforms { a | texCoord : Vec2 } Vec2
texCoord =
    attribute .texCoord "vec2"


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



-- operations


sampleUV : Unit u a Vec2 -> Unit u a Texture -> Unit u a Vec4
sampleUV uv texture =
    func2Unit "texture2D" texture uv


sampleTexture : Unit uniforms { a | texCoord : Vec2 } Texture -> Unit uniforms { a | texCoord : Vec2 } Vec4
sampleTexture texture =
    sampleUV texCoord texture


vec3to4 : Unit u a Vec3 -> Unit u a Vec4
vec3to4 (Unit vec) =
    Unit { vec | source = "vec4(" ++ vec.source ++ ",1.0)" }


xyz : Unit u a Vec4 -> Unit u a Vec3
xyz (Unit vec) =
    Unit { vec | source = vec.source ++ ".xyz" }


normalize : Unit u a Vec3 -> Unit u a Vec3
normalize (Unit u) =
    Unit { u | source = "normalize(" ++ u.source ++ ")" }


mulVectorMat3 : Unit u a Mat3 -> Unit u a Vec3 -> Unit u a Vec3
mulVectorMat3 =
    binOpUnit "*"


extract3by3 : Unit u a Mat4 -> Unit u a Mat3
extract3by3 (Unit m) =
    Unit { m | source = "mat3(" ++ m.source ++ ")" }


mulVector3 : Unit u a Vec3 -> Unit u a Vec3 -> Unit u a Vec3
mulVector3 =
    binOpUnit "*"


mulVector4 : Unit u a Vec4 -> Unit u a Vec4 -> Unit u a Vec4
mulVector4 =
    binOpUnit "*"


mulVectorMat4 : Unit u a Mat4 -> Unit u a Vec4 -> Unit u a Vec4
mulVectorMat4 =
    binOpUnit "*"


addVec3 : Unit u a Vec3 -> Unit u a Vec3 -> Unit u a Vec3
addVec3 =
    binOpUnit "+"


scaleVec3 : Unit u a Vec3 -> Unit u a Float -> Unit u a Vec3
scaleVec3 =
    binOpUnit "*"


binOpUnit : String -> Unit u1 a1 t1 -> Unit u2 a2 t2 -> Unit u3 a3 t3
binOpUnit op (Unit u1) (Unit u2) =
    mergeUnits u1 u2 ("(" ++ u1.source ++ op ++ u2.source ++ ")")


func2Unit : String -> Unit u1 a1 t1 -> Unit u2 a2 t2 -> Unit u3 a3 t3
func2Unit f (Unit u1) (Unit u2) =
    mergeUnits u1 u2 (f ++ "(" ++ u1.source ++ "," ++ u2.source ++ ")")


mergeUnits :
    { a | attributes : Dict String String, uniforms : Dict String String }
    -> { b | attributes : Dict String String, uniforms : Dict String String }
    -> String
    -> Unit u3 a3 t3
mergeUnits u1 u2 source =
    Unit
        { source = source
        , attributes =
            Dict.union u1.attributes u2.attributes
        , uniforms = Dict.union u1.uniforms u2.uniforms
        }
