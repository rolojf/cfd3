module Api exposing (routes)

import ApiRoute exposing (ApiRoute)
import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import HardCodedData
import Html exposing (Html)
import LanguageTag exposing (LanguageTag, emptySubtags)
import LanguageTag.Country as Country
import LanguageTag.Language
import MimeType
import Pages.Manifest as Manifest
import Pages.Manifest.Category as Category
import Pages.Url as Url
import Route exposing (Route)
import Shared
import Site


routes :
    BackendTask FatalError (List Route)
    -> (Maybe { indent : Int, newLines : Bool } -> Html Never -> String)
    -> List (ApiRoute ApiRoute.Response) -- antes htmlToString era (Html Never -> String)
routes getStaticRoutes htmlToString =
    [ BackendTask.succeed manifest
        |> Manifest.generator Site.canonicalUrl
    ]


manifest : Manifest.Config
manifest =
    let
        iconos =
            [ { src = Url.external "/icon-192.png"
              , sizes = [ ( 192, 192 ) ]
              , mimeType = Just MimeType.Png
              , purposes = [ Manifest.IconPurposeAny ]
              }
            , { src = Url.external "/icon-512.png"
              , sizes = [ ( 512, 512 ) ]
              , mimeType = Just MimeType.Png
              , purposes = [ Manifest.IconPurposeAny ]
              }
            ]
    in
    Manifest.init
        { name = HardCodedData.siteName
        , description = ""
        , startUrl = Route.Index |> Route.toPath
        , icons = iconos
        }
        |> Manifest.withCategories [ Category.business ]
        |> Manifest.withLang
            (LanguageTag.Language.es
                |> LanguageTag.build
                    { emptySubtags
                        | region = Just Country.mx
                    }
            )
