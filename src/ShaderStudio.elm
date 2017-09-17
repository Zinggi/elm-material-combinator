module ShaderStudio exposing (program)

--module ShaderStudio exposing (Model, Config, Msg, program, view, update, init, subscriptions)

import AnimationFrame
import Dict exposing (Dict)
import Regex exposing (regex)
import Html exposing (Html, Attribute, div, text)
import Html.Attributes as Attr
import Html.Events exposing (on, onInput, onCheck)
import Json.Decode as JD
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector4 as V4 exposing (Vec4)
import Math.Vector3 as V3 exposing (Vec3, vec3)
import Math.Vector2 as V2 exposing (Vec2)
import Task
import WebGL as GL exposing (Shader, Texture, Mesh, Entity)
import WebGL.Texture
import WebGL.Settings exposing (cullFace, front)
import WebGL.Settings.DepthTest as DepthTest
import Mouse
import Window
import Meshes exposing (meshes)
import Game.Resources as Resources exposing (Resources)


program : Config varyings -> Program Never Model Msg
program config =
    Html.program
        { init = init config
        , view = view config
        , subscriptions = subscriptions
        , update = update
        }


init : Config v -> ( Model, Cmd Msg )
init config =
    ( initModel config, initCmd config )



-- Config


type alias Config varyings =
    { vertexShader : Shader Attributes Uniforms varyings
    , fragmentShader : Shader {} Uniforms varyings
    , defaultTexture : String
    , textures : List String
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
    , currentModel : String
    , zoom : Float
    , resources : Resources
    , diffText : String
    , normText : String
    , isDown : Bool
    , lastMousePos : Mouse.Position
    , mouseDelta : MouseDelta
    , windowSize : Window.Size
    , paused : Bool
    }


type alias MouseDelta =
    { x : Float, y : Float }


initModel : Config v -> Model
initModel config =
    { currentModel = "plane"
    , time = 0
    , zoom = 5
    , resources = Resources.init
    , diffText = config.defaultTexture
    , normText = tryFindNormalMap config
    , isDown = False
    , lastMousePos = Mouse.Position 0 0
    , mouseDelta = MouseDelta 0 (pi / 2)
    , windowSize = Window.Size 800 600
    , paused = True
    }


tryFindNormalMap config =
    config.textures
        |> List.filter (\i -> Regex.contains (Regex.caseInsensitive (regex "norm")) i)
        |> List.head
        |> Maybe.withDefault config.defaultTexture


initCmd : Config v -> Cmd Msg
initCmd config =
    Cmd.batch
        [ Resources.loadTextures (config.defaultTexture :: config.textures)
            |> Cmd.map Resources
        , Task.perform ResizeWindow Window.size
        ]



-- UPDATE


type Msg
    = Tick Float
    | Zoom Float
    | MouseMove Mouse.Position
    | MouseDown Mouse.Position
    | MouseUp
    | Resources Resources.Msg
    | SelectDiffText String
    | SelectNormText String
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

        SelectMesh mesh ->
            ( { model | currentModel = mesh }, Cmd.none )

        SelectNormText texture ->
            ( { model | normText = texture }, Cmd.none )

        SelectDiffText texture ->
            ( { model | diffText = texture }, Cmd.none )

        Resources msg ->
            ( { model | resources = Resources.update msg model.resources }, Cmd.none )

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


renderModel : Config v -> Model -> Mesh Attributes -> Texture -> Texture -> Entity
renderModel config model mesh textureDiff textureNorm =
    let
        ( camera, view, viewProjection, cameraPos ) =
            getCamera model

        modelM =
            M4.identity

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
        renderCullFace config.vertexShader config.fragmentShader mesh uniforms


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
            vec3 (zoom * sin -mx * sin my) (-zoom * cos my) (zoom * cos -mx * sin my)

        view =
            M4.makeLookAt (position) (vec3 0 0 0) (vec3 0 1 0)
    in
        ( proj, view, M4.mul proj view, position )


view : Config v -> Model -> Html Msg
view config model =
    div []
        [ uiView config model
        , case ( Resources.getTexture model.diffText model.resources, Resources.getTexture model.normText model.resources, Dict.get model.currentModel meshes ) of
            ( Just td, Just tn, Just mesh ) ->
                GL.toHtmlWith [ GL.antialias, GL.depth 1 ]
                    [ onZoom
                    , Attr.width (model.windowSize.width)
                    , Attr.height (model.windowSize.height)
                    , Attr.style [ ( "position", "absolute" ) ]
                    ]
                    [ renderModel config model mesh td tn ]

            err ->
                Html.div [] [ Html.text (toString err) ]
        ]


uiView : Config v -> Model -> Html Msg
uiView config model =
    div [ Attr.style [ ( "position", "absolute" ), ( "z-index", "2" ), ( "backgroundColor", "white" ) ] ]
        [ Html.select [ onInput SelectMesh, Attr.value model.currentModel ]
            (makeOptions (Dict.keys meshes))
        , Html.text "paused: "
        , Html.input
            [ Attr.type_ "checkbox"
            , Attr.checked model.paused
            , onCheck (always TogglePause)
            ]
            []
        , Html.text "diffuse texture: "
        , Html.select [ onInput SelectDiffText, Attr.value model.diffText ]
            (makeOptions (allTextures config))
        , Html.text "normal map"
        , Html.select [ onInput SelectNormText, Attr.value model.normText ]
            (makeOptions (allTextures config))
        ]


allTextures config =
    config.defaultTexture :: config.textures


makeOptions list =
    List.map (\t -> Html.option [ Attr.value t ] [ text t ]) list



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


onZoom : Attribute Msg
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


renderCullFace : Shader a u v -> Shader {} u v -> Mesh a -> u -> Entity
renderCullFace =
    GL.entityWith [ DepthTest.default, cullFace front ]
