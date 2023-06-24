module Route.Index exposing (ActionData, Data, Model, Msg, route)

-- * Imports

import Analytics
import Array exposing (Array)
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Http
import Browser.Dom as Dom
import Effect exposing (Effect)
import ErroresHttp
import FatalError exposing (FatalError)
import Footer
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
import MiCloudinary
import MimeType exposing (MimeType)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import PagesMsg exposing (PagesMsg)
import Route exposing (Route)
import RouteBuilder exposing (App, StatefulRoute)
import Shared
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Svg exposing (Svg, path, svg)
import Svg.Attributes as SvgAttr
import UrlPath exposing (UrlPath)
import View exposing (View)



-- * StatefulRoute routeParams data model msg


type alias Model =
    { verNotificaciones : StatusNotificacion

    -- datos para la Galería
    , showSlider : Bool
    , dirAvance : DirAvanceManual
    , avanzoManual : Bool
    , cualSlideActivo : Int
    , aminar : Amimacion
    , cambia : Int
    , cuantasFotos : Int
    }


type Amimacion
    = Entra
    | Sale


type DirAvanceManual
    = None
    | Izq
    | Der


type StatusNotificacion
    = NoStatusYet
    | ConStatusMostrar
    | ConStatusOcultar


type Msg
    = CierraNoti
    | NoOp
    | AvisadoAnalytics (Result Http.Error String)
      -- Mensajes Galería
    | CheckedGalInView (Result Dom.Error Dom.Element)
    | CheckAgainGalInView
    | WaitToGalAutoRotate
    | PresionoBotonIzq
    | PresionoBotonDer
    | Avanza
    | Retrocede
    | Para


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

      -- Valores de Galería
      , showSlider = False
      , dirAvance = None
      , avanzoManual = False
      , cualSlideActivo = 0
      , aminar = Entra
      , cambia = 0
      , cuantasFotos = Array.length textosGal
      }
    , Effect.CheckIfInView "slider-container" CheckedGalInView
    )



-- * Update


type SliderVisibility
    = ApenasEntrando Float
    | NoVisible
    | VisibleAllRight


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

        -- ** Galería
        CheckedGalInView resultaPos ->
            let
                galInSight actual =
                    actual.element.y < actual.viewport.y + 0.7 * actual.viewport.height

                posicion actual =
                    1.0 - (actual.viewport.y / actual.element.y)

                onView : SliderVisibility
                onView =
                    case resultaPos of
                        Ok pos ->
                            if galInSight pos then
                                VisibleAllRight

                            else
                                ApenasEntrando <| posicion pos

                        _ ->
                            NoVisible
            in
            ( { model
                | showSlider =
                    if onView == VisibleAllRight then
                        True

                    else
                        False
              }
            , case onView of
                ApenasEntrando waitingTime ->
                    Effect.EsperaPues
                        (2000 * waitingTime)
                        CheckAgainGalInView

                NoVisible ->
                    Effect.EsperaPues
                        5000
                        CheckAgainGalInView

                VisibleAllRight ->
                    Effect.EsperaPues
                        5500
                        WaitToGalAutoRotate
            )

        CheckAgainGalInView ->
            ( model
            , Effect.CheckIfInView "slider-container" CheckedGalInView
            )

        WaitToGalAutoRotate ->
            if model.avanzoManual then
                ( { model | avanzoManual = False }
                , Effect.EsperaPues 11000 WaitToGalAutoRotate
                )

            else
                ( model
                , Effect.batch
                    [ Effect.EsperaPues 5500 WaitToGalAutoRotate
                    , Effect.Success
                        (if model.dirAvance == Izq then
                            Retrocede

                         else
                            Avanza
                        )
                    ]
                )

        PresionoBotonIzq ->
            update
                app
                shared
                Retrocede
                { model
                    | dirAvance = Izq
                    , avanzoManual = True
                }

        PresionoBotonDer ->
            update
                app
                shared
                Avanza
                { model
                    | dirAvance = Der
                    , avanzoManual = True
                }

        Avanza ->
            ( { model
                | cambia =
                    if model.showSlider then
                        1

                    else
                        0
                , aminar = Sale
                , showSlider = True
              }
            , if model.showSlider then
                Effect.EsperaPues 1300 Para

              else
                Effect.Success Para
            )

        Retrocede ->
            ( { model
                | cambia =
                    if model.showSlider then
                        -1

                    else
                        0
                , aminar = Sale
                , showSlider = True
              }
            , if model.showSlider then
                Effect.EsperaPues 1300 Para

              else
                Effect.Success Para
            )

        Para ->
            let
                nuevoSlideActivo : Int
                nuevoSlideActivo =
                    model.cualSlideActivo + model.cambia

                nuevoSlideActivoValidado : Int
                nuevoSlideActivoValidado =
                    if nuevoSlideActivo < 0 then
                        model.cuantasFotos - 1

                    else if nuevoSlideActivo == model.cuantasFotos then
                        0

                    else
                        nuevoSlideActivo
            in
            ( { model
                | cualSlideActivo = nuevoSlideActivoValidado
                , aminar = Entra
                , cambia = 0
              }
            , Effect.none
            )


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions routeParams path shared model =
    Sub.none



-- * Data


type alias Beneficios =
    { preHeader : String
    , header : String
    , subHeader : String
    , motivos : List Arts
    }


type alias Arts =
    { cabeza : String
    , nota : String
    }


type alias Data =
    { delMD : ContenidoConDatos }


type alias ContenidoConDatos =
    { title : String
    , description : String
    , menu : View.MenuInfo
    , beneficios : Beneficios
    }


data : BackendTask FatalError Data
data =
    let
        miDecoder : String -> Decoder ContenidoConDatos
        miDecoder elCuerpo =
            Decode.map4 ContenidoConDatos
                (Decode.field "title" Decode.string)
                (Decode.field "description" Decode.string)
                MenuDecoder.opMenuToDecode
                (Decode.field "beneficios" beneDecoder)

        getDataFromMD =
            File.bodyWithFrontmatter
                miDecoder
                (HardCodedData.siteName ++ "/index.md")

        beneDecoder =
            Decode.map4 Beneficios
                (Decode.field "preHeader" Decode.string)
                (Decode.field "header" Decode.string)
                (Decode.field "subHeader" Decode.string)
                (Decode.field "art" artsDecoder
                    |> Decode.list
                    |> Decode.field "motivos"
                )

        artsDecoder =
            Decode.map2 Arts
                (Decode.field "cabeza" Decode.string)
                (Decode.field "nota" Decode.string)
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
        [ viewHero shared.showMenu
        , viewFeatures app.data.delMD.beneficios
        , viewNotificacion shared.usuarioStatus model.verNotificaciones
        , viewGaleria model
        , Footer.footer
        ]
    , withMenu =
        app.data.delMD.menu
    }



-- Notificaciones - modals


assetHero : Array String
assetHero =
    [ "v1683373265/CFD2/20180603_142223_r7rbvh.jpg"
    , "v1683372822/CFD1/IMG-20141116-WA0001_jj7dpm.jpg"
    , "v1683372724/CFD1/045Huejutla08SS_ploc37.jpg"
    , "v1683373348/CFD2/DSC_5751_ck7ipy.jpg"
    , "v1683373181/CFD2/DSC_6624_g89z74.jpg"
    ]
        |> Array.fromList


transfHero : String
transfHero =
    "c_scale,f_auto,q_auto,w_154"


viewHero showMenu =
    div
        [ class "tw relative isolate" ]
        [ svg
            [ SvgAttr.class "tw absolute inset-x-0 top-0 -z-10 h-[64rem] w-full stroke-gray-200 [mask-image:radial-gradient(32rem_32rem_at_center,white,transparent)]"
            , Attr.attribute "aria-hidden" "true"
            ]
            [ Svg.defs []
                [ Svg.pattern
                    [ SvgAttr.id "1f932ae7-37de-4c0a-a8b0-a6e3b4d44b84"
                    , SvgAttr.width "200"
                    , SvgAttr.height "200"
                    , SvgAttr.x "50%"
                    , SvgAttr.y "-1"
                    , SvgAttr.patternUnits "userSpaceOnUse"
                    ]
                    [ path
                        [ SvgAttr.d "M.5 200V.5H200"
                        , SvgAttr.fill "none"
                        ]
                        []
                    ]
                ]
            , svg
                [ SvgAttr.x "50%"
                , SvgAttr.y "-1"
                , SvgAttr.class "tw overflow-visible fill-gray-50"
                ]
                [ path
                    [ SvgAttr.d "M-200 0h201v201h-201Z M600 0h201v201h-201Z M-400 600h201v201h-201Z M200 800h201v201h-201Z"
                    , SvgAttr.strokeWidth "0"
                    ]
                    []
                ]
            , Svg.rect
                [ SvgAttr.width "100%"
                , SvgAttr.height "100%"
                , SvgAttr.strokeWidth "0"
                , SvgAttr.fill "url(#1f932ae7-37de-4c0a-a8b0-a6e3b4d44b84)"
                ]
                []
            ]
        , div
            [ class "tw absolute left-1/2 right-0 top-0 -z-10 -ml-24 transform-gpu overflow-hidden blur-3xl lg:ml-24 xl:ml-48"
            , Attr.attribute "aria-hidden" "true"
            ]
            [ div
                [ class "aspect-[801/1036] w-[50.0625rem] bg-gradient-to-tr from-[#ff80b5] to-[#9089fc] opacity-30"
                , Attr.style "clip-path" "polygon(63.1% 29.5%, 100% 17.1%, 76.6% 3%, 48.4% 0%, 44.6% 4.7%, 54.5% 25.3%, 59.8% 49%, 55.2% 57.8%, 44.4% 57.2%, 27.8% 47.9%, 35.1% 81.5%, 0% 97.7%, 39.2% 100%, 35.2% 81.4%, 97.2% 52.8%, 63.1% 29.5%)"
                ]
                []
            ]
        , div
            [ class "tw overflow-hidden" ]
            [ div
                [ class "tw mx-auto max-w-7xl px-6 pb-32 pt-36 sm:pt-60 lg:px-8 lg:pt-32 lg:-mt-32"
                , class
                    (if showMenu then
                        "tw mt-36 sm:mt-12"

                     else
                        "tw -mt-24 sm:-mt-36"
                    )
                ]
                [ div
                    [ class "tw mx-auto max-w-2xl gap-x-14 lg:mx-0 lg:flex lg:max-w-none lg:items-center" ]
                    [ div
                        [ class "tw w-full max-w-xl lg:shrink-0 xl:max-w-2xl" ]
                        [ Html.h1
                            [ class "tw text-4xl font-bold tracking-tight text-gray-900 sm:text-5xl" ]
                            [ text "Comunidad Familia de Dios" ]
                        , Html.p
                            [ class "tw relative mt-6 text-lg leading-8 text-gray-600 sm:max-w-md sm:mt-20 lg:mt-6 lg:max-w-none" ]
                            [ text "Recibimos la fe en JesuCristo de familias predicando con fuerza y testimonios. Aprendemos a vivir esa fe en comunidades. Y compartimos nuestra fe más delante misionando." ]
                        ]
                    , div
                        [ class "tw mt-14 flex justify-end gap-8 sm:-mt-44 sm:justify-start sm:pl-20 lg:mt-0 lg:pl-0" ]
                        [ div
                            [ class "tw ml-auto w-44 flex-none space-y-8 pt-32 sm:ml-0 sm:pt-80 lg:order-last lg:pt-36 xl:order-none xl:pt-80" ]
                            [ div
                                [ class "relative" ]
                                [ Html.img
                                    [ Attr.src <| MiCloudinary.url transfHero <| Maybe.withDefault "" <| Array.get 0 assetHero
                                    , Attr.alt ""
                                    , class "tw aspect-[2/3] w-full rounded-xl bg-gray-900/5 object-cover shadow-lg"
                                    ]
                                    []
                                , div
                                    [ class "tw pointer-events-none absolute inset-0 rounded-xl ring-1 ring-inset ring-gray-900/10" ]
                                    []
                                ]
                            ]
                        , div
                            [ class "tw mr-auto w-44 flex-none space-y-8 sm:mr-0 sm:pt-52 lg:pt-36" ]
                            [ div
                                [ class "tw relative" ]
                                [ Html.img
                                    [ Attr.src <| MiCloudinary.url transfHero <| Maybe.withDefault "" <| Array.get 1 assetHero
                                    , Attr.alt ""
                                    , class "tw aspect-[2/3] w-full rounded-xl bg-gray-900/5 object-cover shadow-lg"
                                    ]
                                    []
                                , div
                                    [ class "tw pointer-events-none absolute inset-0 rounded-xl ring-1 ring-inset ring-gray-900/10" ]
                                    []
                                ]
                            , div
                                [ class "relative" ]
                                [ Html.img
                                    [ Attr.src <| MiCloudinary.url transfHero <| Maybe.withDefault "" <| Array.get 2 assetHero
                                    , Attr.alt ""
                                    , class "tw aspect-[2/3] w-full rounded-xl bg-gray-900/5 object-cover shadow-lg"
                                    ]
                                    []
                                , div
                                    [ class "tw pointer-events-none absolute inset-0 rounded-xl ring-1 ring-inset ring-gray-900/10" ]
                                    []
                                ]
                            ]
                        , div
                            [ class "tw w-44 flex-none space-y-8 pt-32 sm:pt-0" ]
                            [ div
                                [ class "tw relative" ]
                                [ Html.img
                                    [ Attr.src <| MiCloudinary.url transfHero <| Maybe.withDefault "" <| Array.get 3 assetHero
                                    , Attr.alt ""
                                    , class "tw aspect-[2/3] w-full rounded-xl bg-gray-900/5 object-cover shadow-lg"
                                    ]
                                    []
                                , div
                                    [ class "tw pointer-events-none absolute inset-0 rounded-xl ring-1 ring-inset ring-gray-900/10" ]
                                    []
                                ]
                            , div
                                [ class "tw relative" ]
                                [ Html.img
                                    [ Attr.src <| MiCloudinary.url transfHero <| Maybe.withDefault "" <| Array.get 4 assetHero
                                    , Attr.alt ""
                                    , class "tw aspect-[2/3] w-full rounded-xl bg-gray-900/5 object-cover shadow-lg"
                                    ]
                                    []
                                , div
                                    [ class "tw pointer-events-none absolute inset-0 rounded-xl ring-1 ring-inset ring-gray-900/10" ]
                                    []
                                ]
                            ]
                        ]
                    ]
                ]
            ]
        ]



-- *** View Features


viewFeatures : Beneficios -> Html msg
viewFeatures bene =
    let
        viewArts : Arts -> Html msg
        viewArts articulo =
            div
                [ class "tw relative" ]
                [ Html.dt []
                    [ HeroIcons.outlineCheck
                    , Html.p
                        [ class "tw ml-9 text-lg leading-6 font-medium text-gray-900"
                        ]
                        [ text articulo.cabeza ]
                    ]
                , Html.dd
                    [ class "tw mt-2 ml-9 text-base text-gray-500"
                    ]
                    [ text articulo.nota ]
                ]
    in
    div
        [ class "tw bg-white"
        , Attr.id "features"
        ]
        [ div
            [ class "tw max-w-7xl mx-auto lg:pb-8 px-4 sm:px-6 lg:px-8 lg:grid lg:grid-cols-3 lg:gap-x-8" ]
            [ div []
                [ Html.h2
                    [ class "tw text-base font-semibold text-indigo-600 uppercase tracking-wide" ]
                    [ text bene.preHeader ]
                , Html.p
                    [ class "tw mt-2 text-3xl font-extrabold text-gray-900 font-serif tracking-wide" ]
                    [ text bene.header ]
                , Html.p
                    [ class "tw mt-4 text-lg text-gray-500" ]
                    [ text bene.subHeader ]
                ]
            , div
                [ class "tw mt-12 lg:mt-0 lg:col-span-2" ]
                [ Html.dl
                    [ class "tw space-y-10 sm:space-y-0 sm:grid sm:grid-cols-2 sm:grid-rows-3 sm:grid-flow-col sm:gap-x-6 sm:gap-y-10 lg:gap-x-8" ]
                    (List.map viewArts bene.motivos)
                ]
            ]
        ]



-- *** View Retro


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



-- ** View Galeria


textosGal : Array String
textosGal =
    [ "Oración", "Predica" ]
        |> Array.fromList


fotosGal : Array String
fotosGal =
    [ "v1683370129/CFD2/DSC_4023_vx4jnl.jpg"
    , "v1683370141/CFD2/031Huejutla08SS_gx6zku.jpg"
    , "v1683370128/CFD2/SAM_1134_ssf9kt.jpg"
    , "v1683373602/Kerigma12may07_044_v0ikcc.jpg"
    , "v1683372898/CFD1/DSC_4027_o6f5yq.jpg"
    , "v1683373103/CFD2/20140414_165332_s29faa.jpg"
    , "v1683370124/CFD1/predicando7JulCapillaSnMaVianey2_jvujvt.jpg"
    , "v1683373984/CFD2/2013-03-29-0034_qkj1c5.jpg"
    ]
        |> Array.fromList


transfGal : String
transfGal =
    "c_scale,f_auto,q_auto,w_640"


viewGaleria : Model -> Html (PagesMsg Msg)
viewGaleria modeloDeGal =
    viewGal
        fotosGal
        textosGal
        modeloDeGal


viewGal : Array String -> Array String -> Model -> Html (PagesMsg Msg)
viewGal listadoCompletoImgs textos model =
    div
        [ Attr.id "slider-container"
        , class "slider"
        , class "tw relative flex items-center justify-center h-screen overflow-hidden"
        ]
        [ viewSlider
            model.showSlider
            listadoCompletoImgs
            textos
            model.cualSlideActivo
            model.aminar
        ]


viewSlider : Bool -> Array String -> Array String -> Int -> Amimacion -> Html (PagesMsg Msg)
viewSlider showIt listadoCompletoImgs textos slideActivo animar =
    let
        letraVa : Int -> Animation
        letraVa orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (orden * 70)
                    , Animation.easeInQuint
                    ]
                }
                [ P.opacity 1
                , P.y 0
                ]
                [ P.opacity 0
                , P.y -60.0
                ]

        letraViene : Int -> Animation
        letraViene orden =
            Animation.fromTo
                { duration = 600
                , options =
                    [ Animation.delay (1000 + orden * 70)
                    , Animation.easeOutQuint
                    ]
                }
                [ P.opacity 0
                , P.y 60.0
                ]
                [ P.opacity 1
                , P.y 0
                ]

        fotoVa : Int -> Animation
        fotoVa orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (500 + 120 * orden)
                    , Animation.easeInCubic
                    ]
                }
                [ P.opacity 1
                , P.y 0
                ]
                [ P.opacity 0
                , P.y -600.0
                ]

        fotoViene : Int -> Animation
        fotoViene orden =
            Animation.fromTo
                { duration = 400
                , options =
                    [ Animation.delay (200 * orden)
                    , Animation.easeInCubic
                    ]
                }
                [ P.opacity 0
                , P.y 600
                ]
                [ P.opacity 1
                , P.y 0
                ]

        despliega4 : Array String -> List (Html msg)
        despliega4 subListado =
            Array.toIndexedList subListado
                |> List.foldl
                    -- (a -> b -> b) -> b -> List a -> b -//- tuple  -> Html msg
                    (\( indice, direccion ) listadoAc ->
                        div
                            [ class <| "img img-" ++ String.fromInt (indice + 1) ]
                            [ case animar of
                                Sale ->
                                    Animated.html
                                        Html.img
                                        (fotoVa indice)
                                        [ Attr.src <| MiCloudinary.url transfGal direccion ]
                                        []

                                Entra ->
                                    Animated.html
                                        Html.img
                                        (fotoViene indice)
                                        [ Attr.src <| MiCloudinary.url transfGal direccion ]
                                        []
                            ]
                            :: listadoAc
                    )
                    []

        seccionDeImagenes desdeCual =
            div
                [ class "imgs" ]
                [ div
                    [ class "grid" ]
                  <|
                    despliega4 <|
                        Array.slice
                            desdeCual
                            (desdeCual + 4)
                            listadoCompletoImgs
                ]

        seccionTexto =
            div
                [ class "tw absolute bottom-0 mb-12 left-0 z-10 flex items-center justify-center font-bold text-9xl leading-5" ]
                [ div
                    [ class "tw text-blue-900" ]
                    (textos
                        |> Array.get slideActivo
                        |> Maybe.withDefault ""
                        |> String.toList
                        |> List.indexedMap
                            (\indice letra ->
                                case animar of
                                    Sale ->
                                        Animated.html
                                            Html.span
                                            (letraVa indice)
                                            []
                                            [ text (String.fromChar letra) ]

                                    Entra ->
                                        Animated.html
                                            Html.span
                                            (letraViene indice)
                                            []
                                            [ text (String.fromChar letra) ]
                            )
                    )
                ]
    in
    div
        [ class "nav" ]
        [ div
            [ class "next"
            , Event.onClick (PagesMsg.fromMsg PresionoBotonDer)
            ]
            []
        , div
            [ class "prev"
            , Event.onClick (PagesMsg.fromMsg PresionoBotonIzq)
            ]
            []
        , if showIt then
            div
                [ class "item" ]
                [ seccionDeImagenes (4 * slideActivo)
                , seccionTexto
                ]

          else
            div [ class "item" ]
                [ seccionTexto
                ]
        ]
