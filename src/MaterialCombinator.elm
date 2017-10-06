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


type alias BlinnPhongConfig u a =
    { diffuse : Unit (BlinnPhongUniforms u) (BlinnPhongAttributes a) Vec3
    , normal : Unit (BlinnPhongUniforms u) (BlinnPhongAttributes a) Vec3
    }


blinnPhongMaterial : BlinnPhongConfig u a -> Material (BlinnPhongAttributes a) (BlinnPhongUniforms u)
blinnPhongMaterial config =
    let
        ( varyingDeclarations, varyingAssignments ) =
            getMoreVaryings [ config.diffuse, config.normal ]
    in
        Material
            { vert =
                "precision mediump float;\n"
                    ++ "attribute vec3 position;\n"
                    ++ "attribute vec3 normal;\n"
                    ++ "attribute vec4 tangent;\n"
                    ++ getAttributes config.normal config.diffuse
                    ++ varyingDeclarations
                    ++ "varying vec3 vLightDirection;\n"
                    ++ "varying vec3 vViewDirection;\n"
                    ++ "uniform mat4 modelMatrix, normalMatrix, modelViewProjectionMatrix;\n"
                    ++ "uniform vec3 lightPosition;\n"
                    ++ "uniform vec3 viewPosition;\n"
                    ++ "mat3 transpose(mat3 m) {\n"
                    ++ "    return mat3(m[0][0], m[1][0], m[2][0],\n"
                    ++ "                m[0][1], m[1][1], m[2][1],\n"
                    ++ "                m[0][2], m[1][2], m[2][2]);\n"
                    ++ "}\n"
                    ++ "void main() {\n"
                    ++ "    vec4 pos = vec4(position, 1.0 );\n"
                    ++ "    vec4 posWorld4 = modelMatrix * pos;\n"
                    ++ "    vec3 posWorld = posWorld4.xyz / posWorld4.w;\n"
                    --      normal mapping
                    ++ "    mat3 normalMat3 = mat3(normalMatrix);\n"
                    ++ "    vec3 n = normalize(normalMat3 * normal);\n"
                    ++ "    vec3 t = normalize(normalMat3 * tangent.xyz);\n"
                    ++ "    vec3 b = normalize(normalMat3 * (cross(normal, tangent.xyz) * tangent.w));\n"
                    ++ "    mat3 tbn = transpose(mat3(t, b, n));\n"
                    ++ "    vLightDirection = tbn*(lightPosition - posWorld);\n"
                    ++ "    vViewDirection = tbn*(viewPosition - posWorld);"
                    ++ varyingAssignments
                    ++ "    gl_Position = modelViewProjectionMatrix * pos;\n"
                    ++ "}\n"
            , frag =
                "precision mediump float;\n"
                    ++ "varying vec3 vLightDirection;\n"
                    ++ "varying vec3 vViewDirection;\n"
                    -- TODO: make these as inputs
                    ++ "const vec3 ambientColor = vec3(0.2, 0.2, 0.2);\n"
                    ++ "const vec3 specColor = vec3(1.0, 1.0, 1.0);\n"
                    ++ "const float shininess = 32.0;\n"
                    -- could also be vec3
                    ++ "const float lightIntensity = 1.0;\n"
                    ++ "const float specularIntensity = 1.0;\n"
                    ++ "const float lightAttenuation = 0.3;\n"
                    ++ getMoreUniforms [ config.diffuse, config.normal ]
                    ++ varyingDeclarations
                    ++ "void main() {\n"
                    ++ varyingsToAttrHack [ config.diffuse, config.normal ]
                    ++ "    vec3 lightDir = normalize(vLightDirection);\n"
                    --      Local normal, in tangent space
                    ++ ("    vec3 pixelNormal = normalize((" ++ getExpression config.normal ++ ")*2.0 - 1.0);\n")
                    --      The lightDir is in tangent space and already contains the vertex normal information!
                    ++ "    float lambert = max(dot(pixelNormal, lightDir), 0.0);\n"
                    --      diffuse + lambert
                    ++ ("    vec3 diffuseColor = " ++ getExpression config.diffuse ++ ";\n")
                    ++ "    vec3 diffuse = lambert * diffuseColor * lightIntensity;\n"
                    --      specular
                    ++ "    vec3 viewDir = normalize(vViewDirection);\n"
                    ++ "    vec3 reflectDir = reflect(-lightDir, pixelNormal);\n"
                    ++ "    vec3 halfwayDir = normalize(lightDir + viewDir);\n"
                    ++ "    float spec = pow(max(dot(pixelNormal, halfwayDir), 0.0), shininess);\n"
                    ++ "    vec3 specular = vec3(spec) * specularIntensity;\n"
                    --      attenuation
                    ++ "    float attenuation = 1.0 / (1.0 + lightAttenuation * pow(length(vLightDirection), 2.0));\n"
                    --      ambient
                    ++ "    vec3 ambient = ambientColor * diffuseColor;\n"
                    --      output
                    ++ "    vec3 final_color = ambient + (diffuse + specular) * attenuation;\n"
                    ++ "    gl_FragColor = vec4(final_color, 1.0);\n"
                    ++ "}\n"
            }



------------------------------------------
-- Implementation
------------------------------------------


getMoreUniforms : List (Unit a u t) -> String
getMoreUniforms l =
    mergeUnitsNoCode l
        |> getUniforms


getMoreVaryings : List (Unit a u t) -> ( String, String )
getMoreVaryings l =
    mergeUnitsNoCode l
        |> getVaryings


varyingsToAttrHack : List (Unit a u t) -> String
varyingsToAttrHack l =
    mergeUnitsNoCode l
        |> varyingToAttrHack


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


mergeUnitsNoCode units =
    List.foldl
        (\(Unit u) (Unit u_) ->
            Unit
                { source = ""
                , attributes = Dict.union u_.attributes u.attributes
                , uniforms = Dict.union u_.uniforms u.uniforms
                }
        )
        emptyNode
        units
