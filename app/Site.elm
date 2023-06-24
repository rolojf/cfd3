module Site exposing (canonicalUrl, config)

import BackendTask exposing (BackendTask)
import FatalError exposing (FatalError)
import HardCodedData
import Head
import MimeType
import Pages.Url as Url
import SiteConfig exposing (SiteConfig)


canonicalUrl : String
canonicalUrl =
    HardCodedData.canonicalUrl


config : SiteConfig
config =
    { canonicalUrl = canonicalUrl
    , head = head
    }


head : BackendTask FatalError (List Head.Tag)
head =
    [ Head.metaName "viewport" (Head.raw "width=device-width,initial-scale=1")
    , Head.icon [ ( 32, 32 ) ] MimeType.Png (Url.external "/favicon.ico")
    , Head.icon [] (MimeType.OtherImage "svg+xml") (Url.external "/icon.svg")
    , Head.appleTouchIcon (Just 180) (Url.external "/apple-touch-icon.png")

    -- , Head.sitemapLink "/sitemap.xml"
    ]
        |> BackendTask.succeed
