module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (fromInt)
import Url exposing (Url)
import Url.Parser exposing ((</>), parse)



-- MAIN


main : Program { port_ : String } Model Msg
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
    , apiEndpoint : String
    , feeds : RemoteData String (List Feed)
    , latestDigest : RemoteData String Digest
    , digests : RemotePaginatedData String (List String)
    , currPage : Page
    }


type alias PageParams =
    { limit : Int }


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | GotFeedsList (Result String (List Feed))
    | GotDigest (Result String Digest)
    | GotDigests (Result String ( List String, PageParams ))
    | GetDigest PageParams


type alias Feed =
    { id : Int
    , url : String
    , title : Maybe String
    }


type RemoteData e a
    = Loading
    | Error e
    | Loaded a


type RemotePaginatedData e a
    = Loading_ PageParams
    | Error_ e PageParams
    | Loaded_ a PageParams


type alias Digest =
    { date : String
    , links : List LinkItem
    }


type alias LinkItem =
    { link : String
    , title : Maybe String
    , feedId : Int
    , feedTitle : Maybe String
    , feedUrl : String
    }


type Page
    = DashboardPage
    | FeedsPage
    | DigestsPage (Maybe String)



-- INIT


init : { port_ : String } -> Url -> Nav.Key -> ( Model, Cmd Msg )
init { port_ } url key =
    let
        initModel =
            { apiEndpoint = "http://localhost:" ++ port_ ++ "/api/v1", key = key, url = url, digests = Loading_ { limit = 10 }, feeds = Loading, latestDigest = Loading, currPage = parseUrl url }
    in
    ( initModel, Cmd.batch [ pageCmdsToRun initModel <| parseUrl url ] )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Cmd.batch
                        [ Nav.pushUrl model.key (Url.toString url)
                        ]
                    )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                page =
                    parseUrl url

                newModel =
                    { model | url = url, currPage = parseUrl url }
            in
            ( newModel
            , pageCmdsToRun newModel page
            )

        GotFeedsList res ->
            case res of
                Err e ->
                    ( { model | feeds = Error e }, Cmd.none )

                Ok feeds ->
                    ( { model | feeds = Loaded feeds }, Cmd.none )

        GotDigests res ->
            case res of
                Err e ->
                    ( { model | digests = Error_ e (getPageParams model.digests) }, Cmd.none )

                Ok ( digests, newPageParams ) ->
                    ( { model | digests = Loaded_ digests newPageParams }, Cmd.none )

        GotDigest res ->
            case res of
                Err e ->
                    ( { model | latestDigest = Error e }, Cmd.none )

                Ok digest ->
                    ( { model | latestDigest = Loaded digest }, Cmd.none )

        GetDigest pageParams ->
            ( model, getDigests model pageParams )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEWS


viewLinkGroup : ( ( String, String ), List LinkItem ) -> Html Msg
viewLinkGroup ( group, links ) =
    div []
        [ details []
            [ summary [ class "cursor-pointer" ]
                [ text <|
                    Tuple.second group
                        ++ "("
                        ++ fromInt (List.length links)
                        ++ ")"
                ]
            , div [ class "mb-6 mt-3 flex flex-col gap-y-2.5" ] <| List.map viewLink links
            ]
        ]


viewLatestDigest : RemoteData String Digest -> Html Msg
viewLatestDigest r =
    case r of
        Loading ->
            div [] [ text "Loading..." ]

        Error e ->
            div [] [ text e ]

        Loaded digest ->
            div [ class "flex flex-col gap-y-2.5" ] [ viewDigest digest ]


viewLink : LinkItem -> Html Msg
viewLink { link, title, feedTitle } =
    a [ href link, target "_blank", class "text-black! flex flex-col gap-y-0.5 p-3 rounded border border-slate-200 cursor-pointer hover:bg-blue-50" ]
        [ a [ href link, target "_blank" ] [ text (Maybe.withDefault link title) ]
        , span [ class "text-xs flex gap-x-2" ]
            [ span
                [ class
                    (if Maybe.Nothing == feedTitle then
                        "hidden"

                     else
                        ""
                    )
                ]
                [ text (Maybe.withDefault "" feedTitle) ]
            , span
                [ class
                    (if feedTitle == Maybe.Nothing then
                        "hidden"

                     else
                        ""
                    )
                ]
                [ text "•" ]
            , span [] [ text <| showUrl link ]
            ]
        ]


viewDigestsList : Model -> Html Msg
viewDigestsList model =
    case model.digests of
        Loading_ _ ->
            div [] [ text "Loading..." ]

        Error_ e _ ->
            div [] [ text e ]

        Loaded_ ls { limit } ->
            div [ class "flex flex-col gap-y-2" ] <|
                (List.map (\s -> a [ class "inline-flex", href (String.join "/" [ "/digests", s ]) ] [ text <| toFriendlyDate s ]) ls
                    ++ [ span [ class "text-blue-600 cursor-pointer", onClick (GetDigest { limit = limit + 10 }) ] [ text "Load more..." ] ]
                )


viewDigest : Digest -> Html Msg
viewDigest { date, links } =
    let
        grouped =
            groupDigestLinksByFeedUrl links
    in
    div [ class "flex flex-col gap-y-2" ]
        [ h3 [] [ text (toFriendlyDate date) ]
        , div [ class "flex flex-col gap-y-1.5" ] <|
            List.map viewLinkGroup (Dict.toList grouped)
        ]


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
    let
        title =
            case model.currPage of
                DashboardPage ->
                    "rdigest dashboard"

                FeedsPage ->
                    "rdigest feeds"

                DigestsPage _ ->
                    "rdigest digest"
    in
    { title = title
    , body =
        [ div [ class "flex flex-col gap-y-4" ]
            [ h1 [] [ a [ href "/" ] [ text "rdigest — dashboard" ] ]
            , div [ class "flex gap-x-3" ]
                [ a [ href "/digests" ] [ text "View digests" ]
                , span [] [ text <| "•" ]
                , a [ href "/feeds" ] [ text "View/manage feeds" ]
                ]
            , hr [] []
            , div []
                [ case model.currPage of
                    DashboardPage ->
                        viewLatestDigest model.latestDigest

                    FeedsPage ->
                        div [] [ text "Feeds page TODO" ]

                    DigestsPage d ->
                        case d of
                            Nothing ->
                                viewDigestsList model

                            Just _ ->
                                viewLatestDigest model.latestDigest
                ]
            ]
        ]
    }



-- UTILS/HELPERS


encodePageParams : PageParams -> Encode.Value
encodePageParams { limit } =
    Encode.object [ ( "limit", Encode.int limit ) ]


getPageParams : RemotePaginatedData e a -> PageParams
getPageParams r =
    case r of
        Loading_ l ->
            l

        Error_ _ a ->
            a

        Loaded_ _ a ->
            a


pageCmdsToRun : Model -> Page -> Cmd Msg
pageCmdsToRun model page =
    case page of
        DashboardPage ->
            Cmd.batch [ getDigestForDate model Nothing ]

        FeedsPage ->
            Cmd.batch [ getFeedsList model ]

        DigestsPage d ->
            Cmd.batch
                [ if d == Nothing then
                    getDigests model { limit = Basics.clamp 10 10000 (getPageParams model.digests).limit }

                  else
                    getDigestForDate model d
                ]


parseUrl : Url -> Page
parseUrl url =
    case
        parse
            (Url.Parser.oneOf
                [ Url.Parser.map DashboardPage Url.Parser.top
                , Url.Parser.map FeedsPage (Url.Parser.s "feeds")
                , Url.Parser.map (DigestsPage Nothing) (Url.Parser.s "digests")
                , Url.Parser.map (\p -> DigestsPage (Just p)) (Url.Parser.s "digests" </> Url.Parser.string)
                ]
            )
            url
    of
        Just p ->
            p

        Nothing ->
            DashboardPage


toMonthString : String -> String
toMonthString m =
    case m of
        "01" ->
            "January"

        "02" ->
            "February"

        "03" ->
            "March"

        "04" ->
            "April"

        "05" ->
            "May"

        "06" ->
            "June"

        "07" ->
            "July"

        "08" ->
            "August"

        "09" ->
            "September"

        "10" ->
            "October"

        "11" ->
            "November"

        "12" ->
            "December"

        _ ->
            m


toFriendlyDate : String -> String
toFriendlyDate dateString =
    let
        parts =
            String.split "-" dateString
    in
    case parts of
        year :: month :: date :: _ ->
            String.join "" [ date, ", ", toMonthString month, " ", year ]

        _ ->
            dateString


groupDigestLinksByFeedUrl : List LinkItem -> Dict ( String, String ) (List LinkItem)
groupDigestLinksByFeedUrl =
    let
        go : Dict ( String, String ) (List LinkItem) -> List LinkItem -> Dict ( String, String ) (List LinkItem)
        go dict links =
            case links of
                [] ->
                    dict

                h :: rest ->
                    go
                        (Dict.update ( h.feedUrl, Maybe.withDefault h.feedUrl h.feedTitle )
                            (\v ->
                                case v of
                                    Nothing ->
                                        Just [ h ]

                                    Just v_ ->
                                        Just <| (v_ ++ [ h ])
                            )
                            dict
                        )
                        rest
    in
    go Dict.empty


showUrl : String -> String
showUrl url =
    let
        parsed =
            Url.fromString url
    in
    case parsed of
        Nothing ->
            url

        Just u ->
            u.host ++ u.path ++ Maybe.withDefault "" u.query ++ Maybe.withDefault "" u.fragment


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
        date =
            Decode.field "date" Decode.string

        links =
            Decode.field "links" (Decode.list linkDecoder)

        linkDecoder =
            Decode.map5 (\l t fId fTitle fUrl -> { link = l, title = t, feedId = fId, feedTitle = fTitle, feedUrl = fUrl })
                (Decode.field "link" Decode.string)
                (Decode.field "title" (Decode.maybe Decode.string))
                (Decode.field "feed_id" Decode.int)
                (Decode.field "feed_title" (Decode.maybe Decode.string))
                (Decode.field "feed_url" Decode.string)
    in
    Decode.map2 (\d ls -> { date = d, links = ls }) date links



-- API CALLS


getFeedsList : Model -> Cmd Msg
getFeedsList model =
    Http.post
        { url = model.apiEndpoint
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


getDigestForDate : Model -> Maybe String -> Cmd Msg
getDigestForDate model maybeDate =
    Http.post
        { url = model.apiEndpoint
        , body = Http.jsonBody (makePostBody "digests" "get" (Maybe.map Encode.string maybeDate))
        , expect =
            Http.expectJson
                (\res ->
                    case res of
                        Err e ->
                            GotDigest (Err <| httpErrorToString e)

                        Ok f ->
                            GotDigest (Ok f)
                )
                digestDecoder
        }


getDigests : Model -> PageParams -> Cmd Msg
getDigests model pageParams =
    Http.post
        { url = model.apiEndpoint
        , body = Http.jsonBody (makePostBody "digests" "list" (Just (encodePageParams pageParams)))
        , expect =
            Http.expectJson
                (\res ->
                    case res of
                        Err e ->
                            GotDigests (Err <| httpErrorToString e)

                        Ok f ->
                            GotDigests (Ok f)
                )
                (Decode.map2 (\a b -> ( a, b )) (Decode.list Decode.string) (Decode.succeed pageParams))
        }
