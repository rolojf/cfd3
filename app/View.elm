module View exposing
    ( View, map
    , Liga, LigaTipo(..), MenuInfo(..)
    )

{-|

@docs View, map

-}

import Html exposing (Html)
import UrlPath exposing (UrlPath)
import Route exposing (Route)


{-| -}
type alias View msg =
    { title : String
    , body : List (Html msg)
    , withMenu : MenuInfo
    }


type MenuInfo
    = NoMenu
    | SiMenu (List Liga)


type alias Liga =
    { dir : LigaTipo
    , queDice : String
    , especial : Bool
    }


type LigaTipo
    = Otra UrlPath
    | Interna Route


{-| -}
map : (msg1 -> msg2) -> View msg1 -> View msg2
map fn doc =
    { title = doc.title
    , body = List.map (Html.map fn) doc.body
    , withMenu = doc.withMenu
    }
