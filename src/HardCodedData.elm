module HardCodedData exposing (..)

import LanguageTag.Country
import LanguageTag.Language


dataModContacto : { description : String, title : String }
dataModContacto =
    { description = "Formulario para enviar comentarios, retroalimentar o establecer comunicación con " ++ siteName
    , title = "Para comunicarse con CFD"
    }



-- Para Site.elm


canonicalUrl : String
canonicalUrl =
    "https://comunidadfamiliadedios.org/"



-- imagen a modificar manualmente para cada sitio en módulo contacto


imagen :
    { logoTrans : String
    , logoResource : String
    , altMenuLogo : String
    }
imagen =
    { logoTrans = "f_auto"
    , logoResource = "v1683373048/CFD1/0913ipIMG_0130_rzmkmi.jpg"
    , altMenuLogo = "nada"
    }



{- HARDCODED Site Info para Shared.elm module
   La idea es que el contenido para cada página este en un folder diferente
   que se define con siteName el nombre del folder
-}


siteName : String
siteName =
    "cfd"


localito : Maybe ( LanguageTag.Language.Language, LanguageTag.Country.Country )
localito =
    Just
        ( LanguageTag.Language.es
        , LanguageTag.Country.mx
        )
