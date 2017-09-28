module MaterialCombinator
    exposing
        ( Material
        , customMaterial
        , blinnPhongMaterial
        , render
        , extractShader
        , extractShaderCode
        )

{-|

    TODO: add simpler materials, kinda like:
    https://docs.unrealengine.com/latest/INT/Engine/Rendering/Materials/MaterialProperties/LightingModels/index.html
    https://docs.unrealengine.com/udk/Three/MaterialsOverview.html
    https://docs.unrealengine.com/udk/Three/MaterialBasics.html
    https://docs.unrealengine.com/udk/Three/MaterialExamples.html
-}

import Math.Matrix4 exposing (Mat4)
import Math.Vector4 as V4 exposing (Vec4)
import Math.Vector3 as V3 exposing (Vec3)
import WebGL exposing (Shader, Texture, Mesh, entity)
import Dict exposing (Dict)


--

import MaterialCombinator.Types as MT exposing (Material(..), Unit(..))


type alias Material attributes uniforms =
    MT.Material attributes uniforms


render : Material attributes uniforms -> Mesh attributes -> uniforms -> WebGL.Entity
render material =
    entity (WebGL.unsafeShader (getVert material)) (WebGL.unsafeShader (getFrag material))


type alias ShaderTuple a u v =
    ( Shader a u v, Shader {} u v )


extractShader : Material attributes uniforms -> ShaderTuple attributes uniforms v
extractShader m =
    let
        ( v, f ) =
            extractShaderCode m
    in
        ( WebGL.unsafeShader v, WebGL.unsafeShader f )


extractShaderCode : Material attributes uniforms -> ( String, String )
extractShaderCode (Material m) =
    ( m.vert, m.frag )


getVert : Material a u -> String
getVert (Material a) =
    a.vert


getFrag : Material a u -> String
getFrag (Material a) =
    a.frag


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


type alias BlinnPhongUniforms a =
    { a | projectionMatrix : Mat4, modelViewMatrix : Mat4, normalMatrix : Mat4 }


type alias BlinnPhongAttributes a =
    { a | position : Vec3, normal : Vec3 }



{- TODO: narrow attribute type -}


blinnPhongMaterial : { diffuse : Unit (BlinnPhongUniforms u) (BlinnPhongAttributes a) Vec3 } -> Material (BlinnPhongAttributes a) (BlinnPhongUniforms u)
blinnPhongMaterial config =
    let
        ( varyingDeclarations, varyingAssignments ) =
            getVaryings config.diffuse
    in
        Material
            { vert =
                "precision mediump float;\n"
                    ++ "attribute vec3 position;\n"
                    ++ "attribute vec3 normal;\n"
                    ++ getAttributes emptyNode config.diffuse
                    ++ varyingDeclarations
                    ++ "varying vec3 normalV;\n"
                    ++ "varying vec3 vertPos;\n"
                    ++ "uniform mat4 projectionMatrix, modelViewMatrix, normalMatrix, modelViewProjectionMatrix;\n"
                    ++ "void main() {\n"
                    ++ "    gl_Position = projectionMatrix * modelViewMatrix * vec4(position, 1.0);\n"
                    ++ "    vec4 vertPos4 = modelViewMatrix * vec4(position, 1.0);\n"
                    ++ "    vertPos = vec3(vertPos4) / vertPos4.w;\n"
                    ++ "    normalV = vec3(normalMatrix * vec4(normal, 0.0));\n"
                    ++ varyingAssignments
                    ++ "}\n"
            , frag =
                "precision mediump float;\n"
                    ++ "varying vec3 normalV;\n"
                    ++ "varying vec3 vertPos;\n"
                    -- lightPosition has to be in view space!
                    ++ "uniform vec3 lightPosition;\n"
                    ++ "const vec3 ambientColor = vec3(0.1, 0.0, 0.0);\n"
                    ++ "const vec3 specColor = vec3(1.0, 1.0, 1.0);\n"
                    ++ getMoreUniforms [ config.diffuse ]
                    ++ varyingDeclarations
                    ++ "void main() {\n"
                    ++ varyingToAttrHack config.diffuse
                    ++ "    vec3 normal = normalize(normalV);\n"
                    ++ "    vec3 lightDir = normalize(lightPosition - vertPos);\n"
                    ++ "    float lambertian = max(dot(lightDir, normal), 0.0);\n"
                    ++ "    float specular = 0.0;\n"
                    ++ "    if (lambertian > 0.0) {\n"
                    ++ "        vec3 viewDir = normalize(-vertPos);\n"
                    ++ "        vec3 halfDir = normalize(lightDir + viewDir);\n"
                    ++ "        float specAngle = max(dot(halfDir, normal), 0.0);\n"
                    ++ "        specular = pow(specAngle, 16.0);\n"
                    ++ "    }\n"
                    ++ ("    vec3 color = ambientColor + lambertian * (" ++ getExpression config.diffuse ++ ") + specular * specColor;\n")
                    ++ "    gl_FragColor = vec4(color, 1.0);\n"
                    ++ "}\n"
            }



------------------------------------------
-- Implementation
------------------------------------------


getMoreUniforms : List (Unit a u t) -> String
getMoreUniforms l =
    List.map getUniforms l
        |> String.join ""


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


emptyNode =
    Unit { source = "", uniforms = Dict.empty, attributes = Dict.empty }


getSource : Unit a u t -> String
getSource (Unit u) =
    u.source ++ ";\n"


getExpression : Unit a u t -> String
getExpression (Unit u) =
    u.source


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
