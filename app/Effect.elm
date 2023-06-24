module Effect exposing (Effect(..), batch, fromCmd, map, none, perform)

{-|

@docs Effect, batch, fromCmd, map, none, perform

-}

import Browser.Dom as Dom
import Browser.Navigation
import Form
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import Pages.Fetcher
import Process
import Task
import Url exposing (Url)


{-| -}
type Effect msg
    = None
    | Cmd (Cmd msg)
    | Batch (List (Effect msg))
    | GetStargazers (Result Http.Error Int -> msg)
    | SetField { formId : String, name : String, value : String }
    | FetchRouteData
        { data : Maybe FormData
        , toMsg : Result Http.Error Url -> msg
        }
    | Submit
        { values : FormData
        , toMsg : Result Http.Error Url -> msg
        }
    | SubmitFetcher (Pages.Fetcher.Fetcher msg)
    | EsperaPues Float msg
    | SoloAccedeLiga String (Result Http.Error String -> msg)
    | PushUrl String
    | Enfoca msg String
    | MandaABasin
        { respuestas : Encode.Value
        , toMsg : Result Http.Error String -> msg
        }


{-| -}
type alias RequestInfo =
    { contentType : String
    , body : String
    }


{-| -}
none : Effect msg
none =
    None


{-| -}
batch : List (Effect msg) -> Effect msg
batch =
    Batch


{-| -}
fromCmd : Cmd msg -> Effect msg
fromCmd =
    Cmd


{-| -}
map : (a -> b) -> Effect a -> Effect b
map fn effect =
    case effect of
        None ->
            None

        Cmd cmd ->
            Cmd (Cmd.map fn cmd)

        Batch list ->
            Batch (List.map (map fn) list)

        GetStargazers toMsg ->
            GetStargazers (toMsg >> fn)

        FetchRouteData fetchInfo ->
            FetchRouteData
                { data = fetchInfo.data
                , toMsg = fetchInfo.toMsg >> fn
                }

        Submit fetchInfo ->
            Submit
                { values = fetchInfo.values
                , toMsg = fetchInfo.toMsg >> fn
                }

        SetField info ->
            SetField info

        SubmitFetcher fetcher ->
            fetcher
                |> Pages.Fetcher.map fn
                |> SubmitFetcher

        SoloAccedeLiga dire toMsg ->
            SoloAccedeLiga dire (toMsg >> fn)

        EsperaPues cuantoEsperar msg ->
            EsperaPues cuantoEsperar <| fn msg

        PushUrl dire ->
            PushUrl dire

        Enfoca msg queID ->
            Enfoca (fn msg) queID

        MandaABasin someInfo ->
            MandaABasin
                { respuestas = someInfo.respuestas
                , toMsg = someInfo.toMsg >> fn
                }


{-| -}
perform :
    { fetchRouteData :
        { data : Maybe FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , submit :
        { values : FormData
        , toMsg : Result Http.Error Url -> pageMsg
        }
        -> Cmd msg
    , runFetcher :
        Pages.Fetcher.Fetcher pageMsg
        -> Cmd msg
    , fromPageMsg : pageMsg -> msg
    , key : Browser.Navigation.Key
    , setField : { formId : String, name : String, value : String } -> Cmd msg
    }
    -> Effect pageMsg
    -> Cmd msg
perform ({ fromPageMsg, key } as helpers) effect =
    case effect of
        None ->
            Cmd.none

        Cmd cmd ->
            Cmd.map fromPageMsg cmd

        SetField info ->
            helpers.setField info

        Batch list ->
            Cmd.batch (List.map (perform helpers) list)

        GetStargazers toMsg ->
            Http.get
                { url =
                    "https://api.github.com/repos/dillonkearns/elm-pages"
                , expect = Http.expectJson (toMsg >> fromPageMsg) (Decode.field "stargazers_count" Decode.int)
                }

        FetchRouteData fetchInfo ->
            helpers.fetchRouteData
                fetchInfo

        Submit record ->
            helpers.submit record

        SubmitFetcher record ->
            helpers.runFetcher record

        EsperaPues cuantosMS toMsg ->
            Task.perform
                (\() -> fromPageMsg toMsg)
                (Process.sleep cuantosMS)

        SoloAccedeLiga direccion toMsg ->
            Http.get
                { url = direccion
                , expect = Http.expectString (toMsg >> fromPageMsg)
                }

        PushUrl dir ->
            Browser.Navigation.pushUrl
                key
                dir

        Enfoca toMsg queId ->
            Task.attempt
                (\_ -> fromPageMsg toMsg)
                (Dom.focus queId)

        MandaABasin infoPasada ->
            Http.post
                { url = "https://usebasin.com/f/41489cfac434"
                , body = Http.jsonBody infoPasada.respuestas
                , expect =
                    Http.expectString
                        (infoPasada.toMsg >> fromPageMsg)
                }


type alias FormData =
    { fields : List ( String, String )
    , method : Form.Method
    , action : String
    , id : Maybe String
    }
