module MaterialPower exposing (..)

{-| -}

import Math.Matrix4 exposing (Mat4)
import Math.Vector4 exposing (Vec4)
import Math.Vector3 exposing (Vec3)
import Math.Vector2 exposing (Vec2)
import WebGL exposing (Shader, Texture, Mesh, entity)


{-| this is how the library might be used
-}
example =
    customMaterial
        { position = glMulVectorMat4 (uniform .mvp) (glVec3to4 (attribute .position))
        }
        { fragColor = uniform .color
        }


render : Material attributes uniforms -> Mesh attributes -> uniforms -> WebGL.Entity
render material =
    entity (WebGL.unsafeShader (getVert material)) (WebGL.unsafeShader (getFrag material))


getVert : Material a u -> String
getVert =
    Debug.crash "todo"


getFrag : Material a u -> String
getFrag =
    Debug.crash "todo"


{-| This type just bundles a vertex/fragment shader pair
-}
type Material attributes uniforms
    = Material


{-| This type represents a wire in UE4's material editor
-}
type Unit uniforms attributes type_
    = Unit


{-| create an input variable
-}
uniform : (uniforms -> a) -> Unit uniforms attributes a
uniform =
    Debug.crash "todo"


attribute : (attributes -> a) -> Unit uniforms attributes a
attribute =
    Debug.crash "todo"


customMaterial :
    { position : Unit uniforms attributes Vec4 }
    -> { fragColor : Unit uniforms attributes Vec4 }
    -> Material uniforms attributes
customMaterial =
    Debug.crash "todo"


glMulVectorMat4 : Unit u a Mat4 -> Unit u a Vec4 -> Unit u a Vec4
glMulVectorMat4 =
    Debug.crash "todo"


glVec3to4 : Unit u a Vec3 -> Unit u a Vec4
glVec3to4 =
    Debug.crash "todo"
