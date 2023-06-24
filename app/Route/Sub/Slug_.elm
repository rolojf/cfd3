module Route.Sub.Slug_ exposing (ActionData, Data, Model, Msg, route)

import BackendTask exposing (BackendTask)
import BackendTask.File as File
import BackendTask.Glob as Glob
import Dict exposing (Dict)
import FatalError exposing (FatalError)
import HardCodedData
import Head
import Head.Seo as Seo
import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Html.Parser
import Html.Parser.Util
import Json.Decode as Decode exposing (Decoder)
import Markdown.Block
import MdConverter
import MenuDecoder
import Pages.PageUrl exposing (PageUrl)
import Pages.Url as Url
import PagesMsg exposing (PagesMsg)
import Parser
import UrlPath exposing (UrlPath)
import Route exposing (Route)
import RouteBuilder exposing (App, StatelessRoute)
import Shared
import Svg exposing (path)
import View exposing (View)


type alias Model =
    {}


type alias Msg =
    ()


type alias RouteParams =
    { slug : String
    }


route : StatelessRoute RouteParams Data ActionData
route =
    RouteBuilder.preRender
        { head = head
        , pages = pages
        , data = data
        }
        |> RouteBuilder.buildNoState { view = view }


type FileType
    = Md
    | Html_


type alias MDFile =
    { slug : String
    , tipo : FileType
    , filePath : String
    }


allMDFiles : BackendTask FatalError (List MDFile)
allMDFiles =
    Glob.succeed MDFile
        |> Glob.match (Glob.literal (HardCodedData.siteName ++ "/"))
        |> Glob.capture Glob.wildcard
        |> Glob.match (Glob.literal ".")
        |> Glob.capture
            (Glob.oneOf
                ( ( "md", Md )
                , [ ( "html", Html_ ) ]
                )
            )
        |> Glob.captureFilePath
        |> Glob.toBackendTask


pages : BackendTask FatalError (List RouteParams)
pages =
    allMDFiles
        |> BackendTask.map
            (List.map
                (\cadaMDFile -> RouteParams cadaMDFile.slug)
            )


type alias ActionData =
    {}


type TipoDeDoc
    = DelMd (Result String (List Markdown.Block.Block))
    | DelHtml (Result String (List Html.Parser.Node))


type alias Data =
    { body : TipoDeDoc
    , title : String
    , menu : View.MenuInfo
    , description : String
    }


type alias DataPrev =
    { body : String
    , title : String
    , menu : View.MenuInfo
    , description : String
    }


data : RouteParams -> BackendTask FatalError Data
data routeParams =
    let
        parsearSegunTipo cualTipo texto =
            case cualTipo of
                Md ->
                    DelMd (MdConverter.parsea texto)

                Html_ ->
                    DelHtml
                        (Html.Parser.run texto
                            |> Result.mapError Parser.deadEndsToString
                        )

        miDecoder elCuerpo =
            Decode.map3 (DataPrev elCuerpo)
                (Decode.field "title" Decode.string)
                MenuDecoder.opMenuToDecode
                (Decode.field "description" Decode.string)

        sacaPath : String -> List MDFile -> String
        {- regresa el path -}
        sacaPath elSlug listadoArchivos =
            listadoArchivos
                |> List.filter
                    (\unArchivo -> unArchivo.slug == elSlug)
                |> List.head
                |> Maybe.map .filePath
                |> Maybe.withDefault "xxx"

        dsPaginaConFrontmatter =
            allMDFiles
                |> BackendTask.andThen
                    (\listadoDePaginas ->
                        File.bodyWithFrontmatter
                            miDecoder
                            (sacaPath routeParams.slug listadoDePaginas)
                            |> BackendTask.allowFatal
                    )

        sacaElTipo : String -> List MDFile -> FileType
        sacaElTipo elSlug listadoArchivos =
            listadoArchivos
                |> List.filter
                    (\unArchivo -> unArchivo.slug == elSlug)
                |> List.head
                |> Maybe.map .tipo
                |> Maybe.withDefault Html_

        dsTipoDePagina =
            allMDFiles
                |> BackendTask.map
                    (sacaElTipo routeParams.slug)
    in
    BackendTask.map2
        (\dPrev dTipo ->
            { body = parsearSegunTipo dTipo dPrev.body
            , title = dPrev.title
            , menu = dPrev.menu
            , description = dPrev.description
            }
        )
        dsPaginaConFrontmatter
        dsTipoDePagina


head : App Data ActionData RouteParams -> List Head.Tag
head app =
    Seo.summary
        { canonicalUrlOverride = Nothing
        , siteName = app.sharedData.siteName
        , image =
            { url = Url.external "TODO"
            , alt = "elm-pages logo"
            , dimensions = Nothing
            , mimeType = Nothing
            }
        , description = app.data.description
        , locale = HardCodedData.localito
        , title = app.data.title
        }
        |> Seo.website


view : App Data ActionData RouteParams -> Shared.Model -> View (PagesMsg ())
view app shared =
    { title = app.data.title
    , body =
        Html.div
            [ class "tw prose prose-headings:font-serif" ]
            (case app.data.body of
                DelMd cuerpoMd ->
                    MdConverter.renderea cuerpoMd

                DelHtml cuerpoHtml ->
                    case cuerpoHtml of
                        Ok nodos ->
                            Html.Parser.Util.toVirtualDom nodos

                        Err errores ->
                            [ div [] [ text errores ] ]
            )
            |> List.singleton
    , withMenu =
        -- View.SiMenu ligas { mainHero = div [] [], afterHero = div [] [] }
        app.data.menu
    }
