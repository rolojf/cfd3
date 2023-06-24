module Route.About exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import Browser.Navigation
import Effect exposing (Effect)
import FatalError exposing (FatalError)
import HardCodedData
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Pages.PageUrl exposing (PageUrl)
import Pages.Url
import PagesMsg exposing (PagesMsg)
import UrlPath exposing (UrlPath)
import RouteBuilder exposing (App, StatefulRoute)
import Shared
import View exposing (View)


type alias Model =
    {}


type Msg
    = NoOp


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
    ( {}, Effect.none )


update : App Data ActionData RouteParams -> Shared.Model -> Msg -> Model -> ( Model, Effect.Effect msg )
update app shared msg model =
    case msg of
        NoOp ->
            ( model, Effect.none )


subscriptions : RouteParams -> UrlPath -> Shared.Model -> Model -> Sub Msg
subscriptions routeParams path shared model =
    Sub.none


type alias Data =
    { description : String
    , title : String
    }


data : BackendTask FatalError Data
data =
    BackendTask.succeed
        { description = "Hoja básica con información sobre Nosotros"
        , title = "Información Sobre Nosotros "
        }


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = app.sharedData.siteName
        , image =
            { url = Pages.Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = app.data.description ++ app.sharedData.siteName
        , locale = HardCodedData.localito
        , title = app.data.title ++ app.sharedData.nosotros
        }
        |> Seo.website


view : App Data ActionData RouteParams -> Shared.Model -> Model -> View.View (PagesMsg Msg)
view app shared model =
    { title = app.data.title
    , body = div [] [ text app.data.description ] |> List.singleton
    , withMenu = View.NoMenu
    }
