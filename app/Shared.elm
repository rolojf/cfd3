module Shared exposing (Data, Model, Msg(..), SharedMsg(..), UsuarioSt(..), template)

import Analytics
import BackendTask exposing (BackendTask)
import BackendTask.File as File
import Dict
import Effect exposing (Effect)
import ErroresHttp
import FatalError exposing (FatalError)
import HeroIcons
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Events as Event
import Http
import Json.Decode as D
import MiCloudinary
import Pages.Flags
import Pages.PageUrl exposing (PageUrl)
import PagesMsg exposing (PagesMsg)
import UrlPath exposing (UrlPath)
import Route exposing (Route)
import SharedTemplate exposing (SharedTemplate)
import Simple.Animation as Animation exposing (Animation)
import Simple.Animation.Animated as Animated
import Simple.Animation.Property as P
import Url
import View exposing (View)


template : SharedTemplate Msg Model Data msg
template =
    { init = init
    , update = update
    , view = view
    , data = data
    , subscriptions = subscriptions
    , onPageChange = Just OnPageChange
    }


type Msg
    = SharedMsg SharedMsg
    | ToggleMenu
    | OnPageChange
        { path : UrlPath
        , query : Maybe String
        , fragment : Maybe String
        }
    | AnalyticsUsoMenuLigaExterna String
    | AvisadoAnalytics (Result Http.Error String)


type UsuarioSt
    = Desconocido
    | Rechazado
    | Conocido (Result Http.Error String)


type SharedMsg
    = NoOp
    | CambiaStatus UsuarioSt
    | ErrorAlNotificar Http.Error


type alias Model =
    { showMenu : Bool
    , showMenuInicial : Bool
    , errorAlNotificar : Maybe Http.Error
    , usuarioStatus : UsuarioSt
    , elHost : PageUrl
    }


init :
    Pages.Flags.Flags
    ->
        Maybe
            { path :
                { path : UrlPath
                , query : Maybe String
                , fragment : Maybe String
                }
            , metadata : route
            , pageUrl : Maybe PageUrl
            }
    -> ( Model, Effect Msg )
init flags maybePagePath =
    ( { showMenu = False
      , showMenuInicial = False
      , errorAlNotificar = Nothing
      , usuarioStatus = Desconocido
      , elHost =
            let
                defaultPageUrl =
                    { protocol = Url.Http
                    , host = "content"
                    , port_ = Just 1234
                    , path = UrlPath.fromString "yo-no-se"
                    , query = Dict.empty
                    , fragment = Nothing
                    }
            in
            case maybePagePath of
                Nothing ->
                    defaultPageUrl

                Just pagina ->
                    Maybe.withDefault
                        defaultPageUrl
                        pagina.pageUrl
      }
    , Effect.none
    )


track : Msg -> Analytics.Event
track msg =
    case msg of
        OnPageChange nuevaPagina ->
            let
                queCambioReportar =
                    nuevaPagina.path
                        -- |> Path.toSegments
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault "index"
                        |> String.append ("cambio-pagina" ++ "-menuliga-interna-")
            in
            Analytics.eventoXReportar
                queCambioReportar

        AnalyticsUsoMenuLigaExterna queLiga ->
            Analytics.eventoXReportar
                queLiga

        _ ->
            Analytics.none


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ToggleMenu ->
            ( { model
                | showMenuInicial = True
                , showMenu = not model.showMenu
              }
            , Effect.none
            )

        OnPageChange _ ->
            ( { model | showMenu = False }
            , Analytics.toEffect
                (track msg)
                AvisadoAnalytics
            )

        SharedMsg mensajePasado ->
            case mensajePasado of
                CambiaStatus nuevoSt ->
                    ( { model | usuarioStatus = nuevoSt }
                    , Effect.none
                    )

                NoOp ->
                    ( model, Effect.none )

                ErrorAlNotificar cualError ->
                    ( { model | errorAlNotificar = Just cualError }
                    , Effect.none
                    )

        AnalyticsUsoMenuLigaExterna _ ->
            ( model
            , Analytics.toEffect
                (track msg)
                AvisadoAnalytics
            )

        AvisadoAnalytics resulto ->
            ( case resulto of
                Err quePaso ->
                    { model | errorAlNotificar = Just quePaso }

                Ok _ ->
                    model
            , Effect.none
            )


subscriptions : UrlPath -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none


type alias Data =
    { siteName : String -- descipción comercial de la página en cuestión para siteName en SEO summary
    , nosotros : String -- descipción alternativa al logo en menú navegación
    }


data : BackendTask FatalError Data
data =
    File.onlyFrontmatter
        yamlDecoder
        "content/shared.yaml"
        |> BackendTask.allowFatal


yamlDecoder : D.Decoder Data
yamlDecoder =
    D.map2 Data
        (D.field "siteName" D.string)
        (D.field "nosotros" D.string)


view : Data -> { path : UrlPath, route : Maybe Route } -> Model -> (Msg -> msg) -> View msg -> { body : List (Html msg), title : String }
view sharedData page model toMsg pageView =
    { body =
        [ Html.div []
            [ case pageView.withMenu of
                View.NoMenu ->
                    div [] []

                View.SiMenu ligasRecibidas ->
                    viewMenu page.route sharedData ligasRecibidas model.showMenu model.showMenuInicial toMsg
            , Html.main_
                [ class "tw max-w-7xl mx-auto px-4 sm:px-6" ]
                pageView.body
            , div [ class "tw text-neutral-100" ] (viewErroresAlNotificar model.errorAlNotificar)
            ]
        ]
    , title = pageView.title
    }


viewMenu : Maybe Route -> Data -> List View.Liga -> Bool -> Bool -> (Msg -> msg) -> Html msg
viewMenu localRoute dataDelYaml ligas menuOpen byeMenu toMsg =
    let
        quePagina : String
        quePagina =
            localRoute
                |> Maybe.map Route.routeToPath
                |> Maybe.withDefault [ "pagina-rara" ]
                |> List.foldr String.append ""

        quePaginaCompuesta : String
        quePaginaCompuesta =
            if String.isEmpty quePagina then
                "pag-index"

            else
                String.append "pag-" quePagina

        ligasNormales =
            List.filter (\liga -> liga.especial == False) ligas

        ligasEspeciales =
            List.filter .especial ligas

        setLink : String -> Html Msg -> View.Liga -> Html msg
        setLink clases htmlHijos liga =
            case liga.dir of
                View.Otra camino ->
                    div
                        [ camino
                            |> UrlPath.toAbsolute
                            |> String.replace "/" "_"
                            |> String.replace ":" "+"
                            |> String.append (quePaginaCompuesta ++ "-menuliga-externa-")
                            |> AnalyticsUsoMenuLigaExterna
                            |> Event.onClick
                        ]
                        [ Html.a
                            [ Attr.href <| UrlPath.toRelative camino
                            , class clases
                            ]
                            [ htmlHijos ]
                        ]
                        |> Html.map toMsg

                View.Interna rutaLiga ->
                    Route.link
                        [ class clases ]
                        [ htmlHijos ]
                        rutaLiga
                        |> Html.map toMsg

        ligaNormalDesk : Html msg
        ligaNormalDesk =
            Html.nav
                [ class "tw hidden md:flex space-x-10" ]
                (List.map
                    (\cadaLiga ->
                        setLink
                            "tw text-base font-medium text-gray-500 hover:text-gray-900"
                            (text cadaLiga.queDice)
                            cadaLiga
                    )
                    ligasNormales
                )

        ligaEspecialDesk : Html msg
        ligaEspecialDesk =
            div
                [ class "tw hidden md:flex items-center justify-end md:flex-1 lg:w-0" ]
                (List.map
                    (\cadaLiga ->
                        setLink
                            "tw ml-8 whitespace-nowrap inline-flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700"
                            (text cadaLiga.queDice)
                            cadaLiga
                    )
                    ligasEspeciales
                )

        ligaNormalMovil =
            List.map
                (\cadaLiga ->
                    setLink
                        "tw -m-3 p-3 flex items-center rounded-md hover:bg-gray-50"
                        (Html.span
                            [ class "tw ml-3 text-base font-medium text-gray-900" ]
                            [ text cadaLiga.queDice ]
                        )
                        cadaLiga
                )
                ligasNormales

        ligaEspecialMovil =
            div []
                (List.map
                    (\cadaLiga ->
                        setLink
                            "tw w-full flex items-center justify-center px-4 py-2 border border-transparent rounded-md shadow-sm text-base font-medium text-white bg-indigo-600 hover:bg-indigo-700"
                            (text cadaLiga.queDice)
                            cadaLiga
                    )
                    ligasEspeciales
                )

        showMovilMenu : Animation
        showMovilMenu =
            if menuOpen then
                Animation.fromTo
                    { duration = 580
                    , options = [ Animation.easeOut ]
                    }
                    [ P.opacity 0, P.scale 0.92 ]
                    [ P.opacity 1, P.scale 1 ]

            else
                Animation.fromTo
                    { duration = 525
                    , options = [ Animation.easeIn ]
                    }
                    [ P.opacity 1, P.scale 1 ]
                    [ P.opacity 0, P.scale 0.92 ]
    in
    div
        [ class "tw relative bg-white"
        ]
        [ div [ class "tw max-w-7xl mx-auto px-4 sm:px-6" ]
            [ div [ class "tw flex justify-between items-center border-b-2 border-gray-100 py-6 md:justify-start md:space-x-10" ]
                [ div [ class "tw flex justify-start lg:w-0 lg:flex-1" ]
                    [ div
                        []
                        [ Html.span
                            [ class "tw sr-only" ]
                            [ text "Workflow" ]
                        , Html.img
                            [ class "tw h-8 w-auto sm:h-10"
                            , Attr.src "/logo.svg"
                            , Attr.alt <| "Logotipo de " ++ dataDelYaml.nosotros
                            ]
                            []
                        ]
                    ]
                , div
                    [ class "tw -mr-2 -my-2 md:hidden" ]
                    [ Html.button
                        [ Attr.type_ "button"
                        , class "tw bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-gray-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                        , Attr.attribute "aria-expanded" "false"
                        , Event.onClick ToggleMenu
                        ]
                        [ Html.span
                            [ class "tw sr-only" ]
                            [ text "Open menu" ]
                        , HeroIcons.outlineMenu
                        ]
                        |> Html.map toMsg
                    ]
                , ligaNormalDesk
                , ligaEspecialDesk
                ]
            ]
        , if byeMenu then
            Animated.div
                showMovilMenu
                [ class "tw absolute top-0 inset-x-0 p-2 transition transform origin-top-right md:hidden" ]
                [ div
                    [ class " tw bg-slate-100 rounded-lg shadow-lg ring-1 ring-black ring-opacity-5 bg-white divide-y-2 divide-gray-50" ]
                    [ div
                        [ class "tw pt-5 pb-6 px-5" ]
                        [ div
                            [ class "tw flex items-center justify-between" ]
                            [ div []
                                [ Html.img
                                    [ class "tw h-8 w-auto"
                                    , Attr.src "/logo.svg"
                                    , Attr.alt "Logotipo"
                                    ]
                                    []
                                ]
                            , div
                                [ class "tw -mr-2" ]
                                [ Html.button
                                    [ Attr.type_ "button"
                                    , Event.onClick ToggleMenu
                                    , class "tw bg-white rounded-md p-2 inline-flex items-center justify-center text-gray-400 hover:text-blue-500 hover:bg-gray-100 focus:outline-none focus:ring-2 focus:ring-inset focus:ring-indigo-500"
                                    ]
                                    [ Html.span
                                        [ class "tw sr-only" ]
                                        [ text "Close menu" ]
                                    , HeroIcons.outlineX
                                    ]
                                    |> Html.map toMsg
                                ]
                            ]
                        , div
                            [ class "tw mt-6" ]
                            [ Html.nav
                                [ class "tw grid gap-y-8" ]
                                ligaNormalMovil
                            ]
                        ]
                    , div
                        [ class "tw py-6 px-5 space-y-6"
                        ]
                        [ ligaEspecialMovil
                        ]
                    ]
                ]

          else
            div [] []
        ]


viewErroresAlNotificar : Maybe Http.Error -> List (Html msg)
viewErroresAlNotificar cualError =
    case cualError of
        Nothing ->
            []

        Just error ->
            [ text <| ErroresHttp.viewHttpError error ]
