module ErroresHttp exposing (..)

import Http

viewHttpError : Http.Error -> String
viewHttpError cualError = case cualError of
                Http.BadUrl urlBad ->
                    "Pero, error en programa, mal el Url " ++ urlBad

                Http.Timeout ->
                    "No respondió el servidor, Intente de nuevo. Tardó demasiado."

                Http.NetworkError ->
                    "Falló el internet."

                Http.BadStatus codigo ->
                    "Servidor regresó error " ++ String.fromInt codigo

                Http.BadBody infoEnviada ->
                    "Problemas con la información " ++ String.left 20 infoEnviada
