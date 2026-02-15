module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (fromInt)
import Url exposing (Url)



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }



-- TYPES


type alias Model =
    { key : Nav.Key
    , url : Url
    , feeds : RemoteData String (List Feed)
    , digests : RemoteData String (List Digest)
    }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | GetFeedsList
    | GotFeedsList (Result String (List Feed))
    | GetDigests
    | GotDigests (Result String (List Digest))


type alias Feed =
    { id : Int
    , url : String
    , title : Maybe String
    }


type RemoteData e a
    = Loading
    | Error e
    | Loaded a


type alias Digest =
    { digest : String
    , links : List { link : String, title : Maybe String }
    }



-- INIT


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key, url = url, digests = Loading, feeds = Loading }, Cmd.batch [ getFeedsList, getDigests ] )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )

        GetFeedsList ->
            ( model, getFeedsList )

        GotFeedsList res ->
            case res of
                Err e ->
                    ( { model | feeds = Error e }, Cmd.none )

                Ok feeds ->
                    ( { model | feeds = Loaded feeds }, Cmd.none )

        GetDigests ->
            ( model, Cmd.none )

        GotDigests res ->
            case res of
                Err e ->
                    ( { model | digests = Error e }, Cmd.none )

                Ok digests ->
                    ( { model | digests = Loaded digests }, Cmd.none )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEWS


viewFeedsList : RemoteData String (List Feed) -> Html Msg
viewFeedsList r =
    case r of
        Loading ->
            div [] [ text "Loading feeds..." ]

        Error e ->
            div [] [ text e ]

        Loaded feeds ->
            div [ class "flex flex-col gap-y-1.5" ] <| List.map (\f -> a [ href f.url ] [ text (Maybe.withDefault f.url f.title) ]) feeds


view : Model -> Browser.Document Msg
view model =
    { title = "rdigest dashboard"
    , body =
        [ div []
            [ h1 [] [ text "rdigest" ]
            , p [] [ text ("Current path: " ++ model.url.path) ]
            , viewFeedsList model.feeds
            ]
        ]
    }



-- UTILS/HELPERS


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadBody str ->
            str

        Http.BadStatus int ->
            "Error: " ++ fromInt int

        Http.NetworkError ->
            "Network error."

        Http.BadUrl url ->
            "Invalid URL: " ++ url

        _ ->
            "Unknown error."


makePostBody : String -> String -> Maybe Encode.Value -> Encode.Value
makePostBody resource action requestData =
    let
        initial =
            [ ( "resource", Encode.string resource )
            , ( "action", Encode.string action )
            ]
    in
    case requestData of
        Nothing ->
            Encode.object initial

        Just v ->
            Encode.object <| initial ++ [ ( "request", v ) ]



-- DECODERS


feedDecoder : Decode.Decoder Feed
feedDecoder =
    let
        id =
            Decode.field "id" Decode.int

        url =
            Decode.field "url" Decode.string

        title =
            Decode.field "title" (Decode.maybe Decode.string)
    in
    Decode.map3 Feed id url title


feedListDecoder : Decode.Decoder (List Feed)
feedListDecoder =
    Decode.list feedDecoder


paramsDecoder : Decode.Decoder { limit : Maybe Int, offset : Maybe Int }
paramsDecoder =
    let
        limit =
            Decode.field "limit" (Decode.maybe Decode.int)

        offset =
            Decode.field "offset" (Decode.maybe Decode.int)
    in
    Decode.map2 (\l o -> { limit = l, offset = o }) limit offset


digestDecoder : Decode.Decoder Digest
digestDecoder =
    let
        digest =
            Decode.field "digest" Decode.string

        links =
            Decode.field "links" (Decode.list linkDecoder)

        linkDecoder =
            Decode.map2 (\l t -> { link = l, title = t }) (Decode.field "link" Decode.string) (Decode.field "title" (Decode.maybe Decode.string))
    in
    Decode.map2 (\d ls -> { digest = d, links = ls }) digest links



-- API CALLS


getFeedsList : Cmd Msg
getFeedsList =
    Http.post
        { url = "http://localhost:5500/api/v1"
        , body = Http.jsonBody (makePostBody "feeds" "list" Nothing)
        , expect =
            Http.expectJson
                (\res ->
                    case res of
                        Err e ->
                            GotFeedsList (Err <| httpErrorToString e)

                        Ok f ->
                            GotFeedsList (Ok f)
                )
                (Decode.field "feeds" feedListDecoder)
        }


getDigests : Cmd Msg
getDigests =
    Http.post
        { url = "http://localhost:5500/api/v1"
        , body = Http.jsonBody (makePostBody "digests" "list" Nothing)
        , expect =
            Http.expectJson
                (\res ->
                    case res of
                        Err e ->
                            GotDigests (Err <| httpErrorToString e)

                        Ok f ->
                            GotDigests (Ok f)
                )
                (Decode.list digestDecoder)
        }
