module Route.Index exposing (ActionData, Data, Model, Msg, route)

-- * Imports

import Analytics
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Http
import Effect exposing (Effect)
import ErroresHttp
import FatalError exposing (FatalError)
import HardCodedData
import Head
import Head.Seo as Seo
import HeroIcons
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Http
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block
import MdConverter
import MenuDecoder
import MimeType exposing (MimeType)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import PagesMsg exposing (PagesMsg)
import UrlPath exposing (UrlPath)
import Route exposing (Route)
import RouteBuilder exposing (App, StatefulRoute)
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import View exposing (View)



-- * StatefulRoute routeParams data model msg


type alias Model =
    { verNotificaciones : StatusNotificacion }


type StatusNotificacion
    = NoStatusYet
    | ConStatusMostrar
    | ConStatusOcultar


type Msg
    = CierraNoti
    | NoOp
    | AvisadoAnalytics (Result Http.Error String)


type alias RouteParams =
    {}


type alias ActionData =
    {}


route : StatefulRoute RouteParams Data ActionData Model Msg
route =
    RouteBuilder.single
        { head = head
        , data = data
        }
        |> RouteBuilder.buildWithLocalState
            { view = view
            , update = update
            , subscriptions = subscriptions
            , init = init
            }


init : App Data ActionData RouteParams -> Shared.Model -> ( Model, Effect.Effect Msg )
init app shared =
    ( { verNotificaciones =
            if shared.usuarioStatus == Shared.Desconocido then
                NoStatusYet

            else
                ConStatusMostrar
      }
    , Effect.none
    )



-- * Update


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect.Effect Msg )
update app shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )

        CierraNoti ->
            ( { model | verNotificaciones = ConStatusOcultar }
            , Analytics.toEffect
                (Analytics.eventoXReportar "cerro-notificacion")
                AvisadoAnalytics
            )

        AvisadoAnalytics resulto ->
            ( model
            , Effect.none
              {- , case resulto of
                 Err quePaso ->
                     Just (Shared.SharedMsg <| Shared.ErrorAlNotificar quePaso)

                 Ok _ ->
                     Nothing
              -}
            )


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions routeParams path shared model =
    Sub.none



-- * Data


type alias Data =
    { delMD : ContenidoConDatos }


type alias ContenidoConDatos =
    { body : Result String (List Markdown.Block.Block)
    , title : String
    , description : String
    , menu : View.MenuInfo
    }


data : BackendTask FatalError Data
data =
    let
        miDecoder : String -> Decoder ContenidoConDatos
        miDecoder elCuerpo =
            Decode.map4 ContenidoConDatos
                (elCuerpo
                    |> MdConverter.parsea
                    |> Decode.succeed
                )
                (Decode.field "title" Decode.string)
                (Decode.field "description" Decode.string)
                MenuDecoder.opMenuToDecode

        getDataFromMD =
            File.bodyWithFrontmatter
                miDecoder
                (HardCodedData.siteName ++ "/index.md")
    in
    BackendTask.map Data
        getDataFromMD
        |> BackendTask.allowFatal


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    let
        logotipo : Seo.Image
        logotipo =
            { url = "logotipo.png" |> UrlPath.fromString |> Pages.Url.fromPath
            , alt = "Sitio oficial de " ++ app.data.delMD.title
            , dimensions = Just { width = 1094, height = 547 }
            , mimeType = Just <| MimeType.Image MimeType.Png
            }
    in
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = app.sharedData.siteName
        , image = logotipo
        , description = app.data.delMD.description
        , locale = HardCodedData.localito
        , title = app.data.delMD.title
        }
        |> Seo.website



-- * View


view : App Data ActionData RouteParams -> Shared.Model -> Model -> View.View (PagesMsg Msg)
view app shared model =
    { title = "elm-pages is running"
    , body =
        [ viewNotificacion shared.usuarioStatus model.verNotificaciones
        , div
            [ class "tw mt-8 prose prose-headings:font-serif" ]
            (MdConverter.renderea app.data.delMD.body)
            |> Html.map (\_ -> PagesMsg.noOp)
        ]
    , withMenu =
        app.data.delMD.menu
    }



-- Notificaciones - modals


respFromPost : Result Http.Error String -> String
respFromPost resp =
    case resp of
        Ok _ ->
            "Registrado Ok, nos comunicaremos pronto."

        Err cualError ->
            ErroresHttp.viewHttpError cualError


viewNotificacion : Shared.UsuarioSt -> StatusNotificacion -> Html (PagesMsg Msg)
viewNotificacion usrStatus verNotif =
    case usrStatus of
        Shared.Conocido respBasin ->
            retroFinal
                "Formulario Recibido"
                (respFromPost respBasin)
                verNotif

        Shared.Rechazado ->
            retroFinal
                "¡Información no registrada!"
                "Era necesario resolver la ecuación."
                verNotif

        Shared.Desconocido ->
            div [] []


notifAppear : StatusNotificacion -> Animation
notifAppear show =
    case show of
        NoStatusYet ->
            Animation.empty

        ConStatusMostrar ->
            Animation.fromTo
                { duration = 750
                , options =
                    [ Animation.delay 1100
                    , Animation.easeOut
                    ]
                }
                [ P.opacity 0, P.scale 0.92 ]
                [ P.opacity 1, P.scale 1 ]

        ConStatusOcultar ->
            Animation.fromTo
                { duration = 125
                , options = [ Animation.easeIn ]
                }
                [ P.opacity 1, P.scale 1, P.y 0.8 ]
                [ P.opacity 0, P.scale 0.92, P.y 0 ]


retroFinal : String -> String -> StatusNotificacion -> Html (PagesMsg Msg)
retroFinal titulo subtitulo debeAparecer =
    Animated.div
        (notifAppear debeAparecer)
        [ Attr.attribute "aria-live" "assertive"
        , class "tw fixed inset-0 flex items-end px-4 py-6 z-20 pointer-events-none sm:p-6 lg:items-center"
        ]
        [ div
            [ class "tw w-full flex flex-col items-center space-y-4z sm:items-start lg:items-end" ]
            [ div
                [ class "tw max-w-sm w-full bg-gray-200 shadow-lg rounded-lg pointer-events-auto ring-1 ring-black ring-opacity-5 overflow-hidden" ]
                [ div
                    [ class "tw p-4" ]
                    [ div
                        [ class "tw flex items-start" ]
                        [ div
                            [ class "tw flex-shrink-0" ]
                            [ HeroIcons.outlineCheckCircle ]
                        , div
                            [ class "tw ml-3 w-0 flex-1 pt-0.5" ]
                            [ Html.p
                                [ class "tw text-sm font-medium text-gray-900" ]
                                [ text titulo ]
                            , Html.p
                                [ class "tw mt-1 text-sm text-gray-500" ]
                                [ text subtitulo ]
                            ]
                        , div
                            [ class "tw ml-4 flex-shrink-0 flex" ]
                            [ Html.button
                                [ class "tw bg-white rounded-md inline-flex text-gray-400 hover:text-gray-500 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
                                , Event.onClick (PagesMsg.fromMsg CierraNoti)
                                ]
                                [ Html.span
                                    [ class "tw sr-only" ]
                                    [ text "Close" ]
                                , HeroIcons.solidX
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]
