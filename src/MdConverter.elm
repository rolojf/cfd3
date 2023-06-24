module MdConverter exposing (myLink, parsea, renderea)

import Html exposing (Html, div, text)
import Html.Attributes as Attr exposing (class)
import Markdown.Block exposing (Block)
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer exposing (defaultHtmlRenderer)
import PagesMsg exposing (PagesMsg)
import Route exposing (link)


parsea : String -> Result String (List Block)
parsea mdStr =
    let
        deadEndsToString deadEnds =
            deadEnds
                |> List.map Markdown.Parser.deadEndToString
                |> String.join "\n"
    in
    mdStr
        |> Markdown.Parser.parse
        |> Result.mapError deadEndsToString


renderea : Result String (List Block) -> List (Html (PagesMsg ()))
renderea resultado =
    let
        resultadoProcesado : Result String (List (Html (PagesMsg ())))
        resultadoProcesado =
            resultado
                |> Result.andThen
                    (Markdown.Renderer.render myRenderer)
    in
    case resultadoProcesado of
        Ok html ->
            html

        Err errors ->
            [ text errors ]


myRenderer : Markdown.Renderer.Renderer (Html (PagesMsg ()))
myRenderer =
    let
        defaultOne =
            Markdown.Renderer.defaultHtmlRenderer
    in
    { defaultOne
        | html = procesaHtml
        , link = myLink
    }


myLink liga contenido =
    let
        posibleRuta =
            Route.urlToRoute { path = liga.destination }
    in
    case liga.title of
        Just title ->
            case posibleRuta of
                Nothing ->
                    Html.a
                        [ Attr.href liga.destination
                        , Attr.title title
                        ]
                        contenido

                Just ruta ->
                    Route.link
                        [ Attr.title title ]
                        contenido
                        ruta

        Nothing ->
            case posibleRuta of
                Nothing ->
                    Html.a
                        [ Attr.href liga.destination ]
                        contenido

                Just ruta ->
                    Route.link
                        []
                        contenido
                        ruta


procesaHtml : Markdown.Html.Renderer (List (Html (PagesMsg ())) -> Html (PagesMsg ()))
procesaHtml =
    let
        showDiv : Maybe String -> Maybe String -> List (Html (PagesMsg ())) -> Html (PagesMsg ())
        showDiv clase identidad hijos =
            div
                [ case clase of
                    Just claseDef ->
                        class claseDef

                    Nothing ->
                        class ""
                , case identidad of
                    Just soyYoMero ->
                        Attr.id soyYoMero

                    Nothing ->
                        Attr.id ""
                ]
                hijos

        showSpan : String -> List (Html (PagesMsg ())) -> Html (PagesMsg ())
        showSpan clase children =
            Html.span
                [ class clase ]
                children
    in
    Markdown.Html.oneOf
        [ Markdown.Html.tag "div"
            showDiv
            |> Markdown.Html.withOptionalAttribute "class"
            |> Markdown.Html.withOptionalAttribute "id"
        , Markdown.Html.tag "span"
            showSpan
            |> Markdown.Html.withAttribute "class"
        ]
