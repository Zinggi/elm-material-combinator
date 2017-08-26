module ShaderStudio exposing (program)

--module ShaderStudio exposing (Model, Config, Msg, program, view, update, init, subscriptions)

import AnimationFrame
import Dict exposing (Dict)
import Html exposing (Html, div, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onInput, onCheck)
import Json.Decode as JD
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector4 as V4 exposing (Vec4)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector2 as V2 exposing (Vec2)
import Task
import WebGL as GL exposing (Shader, Texture)
import WebGL.Texture
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import Mouse
import Window
import OBJ
import OBJ.Types exposing (ObjFile, Mesh(..), MeshWith)


program : Config varyings -> Program Never Model Msg
program config =
    Html.program
        { init = init
        , view = view config
        , subscriptions = subscriptions
        , update = update
        }


init : ( Model, Cmd Msg )
init =
    ( initModel, initCmd )



-- Config


type alias Config varyings =
    { vertexShader : Shader Attributes Uniforms varyings
    , fragmentShader : Shader {} Uniforms varyings
    }


type alias Uniforms =
    { camera : Mat4
    , lightPosition : Vec3
    , modelMatrix : Mat4
    , modelViewProjectionMatrix : Mat4
    , mvMat : Mat4
    , textureDiff : Texture
    , textureNorm : Texture
    , viewPosition : Vec3
    }


type alias Attributes =
    { position : Vec3
    , texCoord : Vec2
    , normal : Vec3
    , tangent : Vec4
    }



-- MODEL


type alias Model =
    { time : Float
    , mesh : Result String (MeshWith Attributes)
    , currentModel : String
    , zoom : Float
    , diffText : Result String GL.Texture
    , normText : Result String GL.Texture
    , isDown : Bool
    , lastMousePos : Mouse.Position
    , mouseDelta : MouseDelta
    , windowSize : Window.Size
    , paused : Bool
    }


type alias MouseDelta =
    { x : Float, y : Float }


initModel : Model
initModel =
    { mesh = Err "loading ..."
    , currentModel = "meshes/elmLogo.obj"
    , time = 0
    , zoom = 5
    , diffText = Err "Loading texture..."
    , normText = Err "Loading texture..."
    , isDown = False
    , lastMousePos = Mouse.Position 0 0
    , mouseDelta = MouseDelta 0 (pi / 2)
    , windowSize = Window.Size 800 600
    , paused = True
    }


models : List String
models =
    [ "meshes/elmLogo.obj"
    ]


initCmd : Cmd Msg
initCmd =
    Cmd.batch
        [ loadModel "meshes/elmLogo.obj"
        , loadTexture "textures/elmLogoDiffuse.png" DiffTextureLoaded
        , loadTexture "textures/elmLogoNorm.png" NormTextureLoaded
        , Task.perform ResizeWindow Window.size
        ]


loadModel : String -> Cmd Msg
loadModel url =
    OBJ.loadMeshWithTangent url (LoadObj url)



-- UPDATE


type Msg
    = Tick Float
    | LoadObj String (Result String (MeshWith Attributes))
    | Zoom Float
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp
    | DiffTextureLoaded (Result String GL.Texture)
    | NormTextureLoaded (Result String GL.Texture)
    | ResizeWindow Window.Size
    | SelectMesh String
    | TogglePause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick dt ->
            ( { model | time = model.time + dt / 1000 }, Cmd.none )

        Zoom dy ->
            ( { model | zoom = max 0.01 (model.zoom + dy / 100) }, Cmd.none )

        SelectMesh url ->
            ( model, loadModel url )

        LoadObj url mesh ->
            -- TODO: remove: this will be used to store the models in a .elm file
            ( { model | mesh = Debug.log "mesh" mesh, currentModel = url }, Cmd.none )

        DiffTextureLoaded t ->
            ( { model | diffText = t }, Cmd.none )

        NormTextureLoaded t ->
            ( { model | normText = t }, Cmd.none )

        MouseDown p ->
            ( { model | isDown = True, lastMousePos = p }, Cmd.none )

        MouseUp ->
            ( { model | isDown = False }, Cmd.none )

        MouseMove p ->
            ( { model | mouseDelta = getDelta p model.lastMousePos model.mouseDelta, lastMousePos = p }, Cmd.none )

        ResizeWindow w ->
            ( { model | windowSize = w }, Cmd.none )

        TogglePause ->
            ( { model | paused = not model.paused }, Cmd.none )



-- VIEW / RENDER


renderModel : Config v -> Model -> GL.Texture -> GL.Texture -> MeshWith Attributes -> GL.Entity
renderModel config model textureDiff textureNorm mesh =
    let
        ( camera, view, viewProjection, cameraPos ) =
            getCamera model

        modelM =
            M4.makeTranslate (vec3 -1 0 0)

        lightPos =
            vec3 (0.5 * cos (2 * model.time)) (1 + 0.5 * sin (2 * model.time)) 0.5

        uniforms =
            { camera = camera
            , mvMat = M4.mul view modelM
            , modelViewProjectionMatrix = M4.mul viewProjection modelM
            , modelMatrix = modelM
            , viewPosition = cameraPos
            , textureDiff = textureDiff
            , textureNorm = textureNorm
            , lightPosition = lightPos
            }
    in
        renderCullFace config.vertexShader config.fragmentShader (GL.indexedTriangles mesh.vertices mesh.indices) uniforms


getCamera : Model -> ( Mat4, Mat4, Mat4, Vec3 )
getCamera { mouseDelta, zoom, windowSize } =
    let
        ( mx, my ) =
            ( mouseDelta.x, mouseDelta.y )

        aspect =
            toFloat windowSize.width / toFloat windowSize.height

        proj =
            M4.makePerspective 45 aspect 0.01 10000

        position =
            vec3 (zoom * sin -mx * sin my) (-zoom * cos my + 1) (zoom * cos -mx * sin my)

        view =
            M4.makeLookAt (position) (vec3 0 1 0) (vec3 0 1 0)
    in
        ( proj, view, M4.mul proj view, position )


view : Config v -> Model -> Html.Html Msg
view config model =
    div []
        [ uiView model
        , case ( model.mesh, model.diffText, model.normText ) of
            ( Ok m, Ok td, Ok tn ) ->
                GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                    [ onZoom
                    , Attr.width (model.windowSize.width)
                    , Attr.height (model.windowSize.height)
                    , Attr.style [ ( "position", "absolute" ) ]
                    ]
                    [ renderModel config model td tn m ]

            err ->
                Html.div [] [ Html.text (toString err) ]
        ]


uiView : Model -> Html Msg
uiView model =
    div [ Attr.style [ ( "position", "absolute" ), ( "z-index", "2" ), ( "backgroundColor", "white" ) ] ]
        [ Html.select [ onInput SelectMesh, Attr.value model.currentModel ]
            (List.map (\t -> Html.option [ Attr.value t ] [ text t ]) models)
        , Html.input
            [ Attr.type_ "checkbox"
            , Attr.checked model.paused
            , onCheck (always TogglePause)
            ]
            [ Html.text "paused?" ]
        ]



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        ((if model.isDown then
            [ Mouse.moves MouseMove ]
          else
            []
         )
            ++ (if model.paused then
                    []
                else
                    [ AnimationFrame.diffs Tick ]
               )
            ++ [ Mouse.downs MouseDown
               , Mouse.ups (\_ -> MouseUp)
               , Window.resizes ResizeWindow
               ]
        )



-- HELPERS


onZoom : Html.Attribute Msg
onZoom =
    on "wheel" (JD.map Zoom (JD.field "deltaY" JD.float))


getDelta : Mouse.Position -> Mouse.Position -> MouseDelta -> MouseDelta
getDelta curr lastP delta =
    MouseDelta (toFloat (curr.x - lastP.x) / 100 + delta.x) (clamp 0.01 pi (toFloat (curr.y - lastP.y) / 100 + delta.y))


loadTexture : String -> (Result String GL.Texture -> msg) -> Cmd msg
loadTexture url msg =
    WebGL.Texture.load url
        |> Task.attempt
            (\r ->
                case r of
                    Ok t ->
                        msg (Ok t)

                    Err e ->
                        msg (Err ("Failed to load texture: " ++ toString e))
            )


renderCullFace : GL.Shader a u v -> GL.Shader {} u v -> GL.Mesh a -> u -> GL.Entity
renderCullFace =
    GL.entityWith [ DepthTest.default, cullFace front ]
