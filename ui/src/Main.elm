port module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import File exposing (File)
import File.Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import String exposing (fromInt)
import Task
import Url exposing (Url)
import Url.Parser exposing ((</>), parse)



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
-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url
    , apiEndpoint : String
    , feeds : RemotePaginatedData String (List Feed)
    , latestDigest : RemoteData String Digest
    , digests : RemotePaginatedData String (List String)
    , currPage : Page
    , digestSearchTerm : String
    }


type alias PageParams =
    { limit : Int }



-- MSG


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url
    | ShowPrompt String Encode.Value
    | ShowConfirm String Encode.Value
    | ShowAlert String
    | GotFeedsList (Result String ( List Feed, PageParams, Int ))
    | GotDigest (Result String Digest)
    | GotDigests (Result String ( List String, PageParams, Int ))
    | GetDigest PageParams
    | GetFeeds PageParams
    | UpdateDigestSearchTerm String
    | RefreshFeed
    | TriggerFileUpload
    | UploadedOpmlFile File
    | ImportFeeds (Result String String)
    | AddFeedResult (Result String ())
    | GotPromptResponse ( String, String )
    | GotConfirmResponse ( String, Encode.Value )
    | NoOp


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
    | Loaded_ a PageParams Int -- Loaded_ (data) (pageparams like limit/offset) (total_count as int)


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


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init () url key =
    let
        initModel =
            { apiEndpoint =
                (if url.protocol == Url.Https then
                    "https://"

                 else
                    "http://"
                )
                    ++ url.host
                    ++ Maybe.withDefault "" (Maybe.map (\p -> ":" ++ fromInt p) url.port_)
                    ++ "/api/v1"
            , key = key
            , url = url
            , digests = Loading_ { limit = 10 }
            , feeds = Loading_ { limit = 10 }
            , latestDigest = Loading
            , currPage = parseUrl url
            , digestSearchTerm = ""
            }
    in
    ( initModel, Cmd.batch [ pageCmdsToRun initModel <| parseUrl url ] )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotConfirmResponse ( key, value ) ->
            case ( key, value ) of
                ( "deleteFeed", url ) ->
                    let
                        id_ =
                            Decode.decodeValue Decode.string url
                    in
                    case id_ of
                        Ok v ->
                            ( model, deleteFeed model v )

                        Err _ ->
                            ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        GotPromptResponse ( key, value ) ->
            let
                _ =
                    Debug.log "kv" ( key, value )
            in
            case key of
                "addFeed" ->
                    ( model, addFeed model value )

                _ ->
                    ( model, Cmd.none )

        AddFeedResult res ->
            case res of
                Ok _ ->
                    ( model, getFeedsList model (getPageParams model.feeds) )

                Err e ->
                    ( model, showAlert e )

        TriggerFileUpload ->
            ( model, triggerFileUpload )

        UploadedOpmlFile file ->
            let
                _ =
                    Debug.log "called" ""

                _ =
                    Debug.log "file" file
            in
            ( model, File.toString file |> Task.mapError Debug.toString |> Task.attempt ImportFeeds )

        ImportFeeds opml ->
            let
                _ =
                    Debug.log "opml" opml
            in
            ( model, Cmd.none )

        RefreshFeed ->
            ( model, Cmd.batch [ refreshFeed model, showAlert "Started refresh in the background. It may take several minutes. Check the terminal (where you started rdigest) for updates." ] )

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

                feeds =
                    case page of
                        FeedsPage ->
                            Loading_ (getPageParams model.feeds)

                        _ ->
                            model.feeds

                digests =
                    case page of
                        DigestsPage _ ->
                            Loading_ (getPageParams model.digests)

                        _ ->
                            model.digests

                newModel =
                    { model | url = url, feeds = feeds, digests = digests, currPage = parseUrl url }
            in
            ( newModel
            , pageCmdsToRun newModel page
            )

        UpdateDigestSearchTerm str ->
            ( { model | digestSearchTerm = str }, Cmd.none )

        ShowPrompt prompt value ->
            ( model, showPrompt ( prompt, value ) )

        ShowConfirm prompt value ->
            ( model, showConfirm ( prompt, value ) )

        ShowAlert m ->
            ( model, showAlert m )

        GotFeedsList res ->
            case res of
                Err e ->
                    ( { model | feeds = Error_ e (getPageParams model.feeds) }, Cmd.none )

                Ok ( feeds, p, total ) ->
                    ( { model | feeds = Loaded_ feeds p total }, Cmd.none )

        GotDigests res ->
            case res of
                Err e ->
                    ( { model | digests = Error_ e (getPageParams model.digests) }, Cmd.none )

                Ok ( digests, newPageParams, total ) ->
                    ( { model | digests = Loaded_ digests newPageParams total }, Cmd.none )

        GotDigest res ->
            case res of
                Err e ->
                    ( { model | latestDigest = Error e }, Cmd.none )

                Ok digest ->
                    ( { model | latestDigest = Loaded digest }, Cmd.none )

        GetDigest pageParams ->
            ( model, getDigests model pageParams )

        GetFeeds pageParams ->
            ( model, getFeedsList model pageParams )



-- SUBS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ getPromptResponse GotPromptResponse, getConfirmResponse GotConfirmResponse ]



-- VIEWS


viewLinkGroup : ( ( String, String ), List LinkItem ) -> Html Msg
viewLinkGroup ( group, links ) =
    div [ class "p-4 border rounded border-slate-200" ]
        [ h3 [ class "font-bold" ]
            [ text <| String.join "" [ Tuple.second group, " (", fromInt (List.length links), ")" ]
            ]
        , ul [ class "mt-3 space-y-4 list-disc list-outside ml-4" ] <| List.map viewLink links
        ]


viewLatestDigest : String -> RemoteData String Digest -> Html Msg
viewLatestDigest searchTerm r =
    case r of
        Loading ->
            div [] [ text "Loading..." ]

        Error e ->
            div [] [ text e ]

        Loaded digest ->
            let
                digestFiltered =
                    Digest digest.date
                        (List.filter
                            (\item ->
                                if String.length (String.trim <| String.toLower searchTerm) == 0 then
                                    True

                                else
                                    [ item.link
                                    , item.feedUrl
                                    , Maybe.withDefault item.link item.title
                                    , Maybe.withDefault item.feedUrl item.feedTitle
                                    ]
                                        |> List.map String.toLower
                                        |> List.any (\s -> String.contains (String.toLower searchTerm) s)
                            )
                            digest.links
                        )
            in
            div [ class "flex flex-col gap-y-2.5" ] [ viewDigest digestFiltered ]


viewLink : LinkItem -> Html Msg
viewLink { link, title } =
    li []
        [ span [ class "flex flex-col gap-y-0.5" ]
            [ a [ href link, target "_blank" ] [ text (Maybe.withDefault link title) ]
            , span [ class "text-xs opacity-70" ]
                [ text <| showUrl link
                ]
            ]
        ]


viewDigestsList : Model -> Html Msg
viewDigestsList model =
    case model.digests of
        Loading_ _ ->
            div [] [ text "Loading..." ]

        Error_ e _ ->
            div [] [ text e ]

        Loaded_ ls { limit } total ->
            if List.length ls > 0 then
                div [ class "flex flex-col gap-y-3" ] <|
                    (List.map (\s -> a [ class "border border-slate-200 rounded-lg p-3", href (String.join "/" [ "/digests", s ]) ] [ text <| toFriendlyDate s ]) ls
                        ++ [ span
                                [ class
                                    (String.join " "
                                        [ "text-blue-600 cursor-pointer"
                                        , if limit >= total then
                                            "hidden"

                                          else
                                            ""
                                        ]
                                    )
                                , onClick (GetDigest { limit = limit + 10000 })
                                ]
                                [ text "Load more..." ]
                           ]
                    )

            else
                div [ class "opacity-60" ]
                    [ text "No digests yet. Ensure you have "
                    , a [ href "/feeds", class "opacity-100" ] [ text "feeds" ]
                    , text " and then try "
                    , span [ onClick <| RefreshFeed, class "opacity-100 cursor-pointer" ] [ text "refreshing the feeds?" ]
                    ]


viewDigest : Digest -> Html Msg
viewDigest { date, links } =
    let
        grouped =
            groupDigestLinksByFeedUrl links
    in
    div [ class "flex flex-col gap-y-4" ]
        [ h3 [] [ text <| String.join "" [ toFriendlyDate date, " ", "(", fromInt <| List.length links, ")" ] ]
        , input [ class "p-2 text-sm border rounded-md border-slate-200 w-full", placeholder "Search in digest...", onInput UpdateDigestSearchTerm ] []
        , div [ class "grid grid-cols-1 lg:grid-cols-3 gap-4" ] <|
            List.map viewLinkGroup (List.reverse <| List.sortBy (\( _, ls ) -> List.length ls) <| Dict.toList grouped)
        ]


viewFeedsList : Model -> Html Msg
viewFeedsList model =
    case model.feeds of
        Loading_ _ ->
            div [] [ text "Loading feeds..." ]

        Error_ e p ->
            div [] [ text e ]

        Loaded_ feeds p total ->
            let
                noFeeds =
                    List.length feeds == 0
            in
            div [ class "flex flex-col gap-y-3" ]
                [ div [ class "flex gap-x-3" ]
                    [ button [ onClick (ShowPrompt "Enter feed URL:" (Encode.object [ ( "key", Encode.string "addFeed" ), ( "defaultValue", Encode.string "" ) ])), class "" ]
                        [ text "+ Add RSS feed" ]
                    , button [ onClick TriggerFileUpload, class "" ]
                        [ text "+ Import OPML" ]
                    ]
                , div
                    [ class
                        ("opacity-60 mt-6 "
                            ++ (if noFeeds then
                                    "hidden"

                                else
                                    ""
                               )
                        )
                    ]
                    [ text <| String.join "" [ "Total: ", fromInt total, " feeds." ] ]
                , div
                    [ class
                        ("pt-6 flex flex-col gap-y-3 "
                            ++ (if noFeeds then
                                    "hidden"

                                else
                                    ""
                               )
                        )
                    ]
                  <|
                    List.map
                        (\f ->
                            div [ class "border rounded-lg border-slate-200 p-3 flex gap-x-4 items-center items-start justify-between" ]
                                [ span [ class "font-semibold" ] [ text (Maybe.withDefault f.url f.title) ]
                                , span [ class "flex items-center gap-x-2" ]
                                    [ span
                                        [ class "hidden cursor-pointer text-xs text-blue-600"
                                        , onClick
                                            (ShowPrompt "Change title to:"
                                                (Encode.object
                                                    [ ( "key", Encode.string "editTitle" )
                                                    , ( "defaultValue", Maybe.withDefault f.url f.title |> Encode.string )
                                                    ]
                                                )
                                            )
                                        ]
                                        [ text "Edit name" ]
                                    , span [ class "hidden" ] [ text "•" ]
                                    , span [ class "cursor-pointer text-xs text-red-500", onClick <| ShowConfirm "Sure? This will delete the feed and the links from it. You can't undo it." (Encode.object [ ( "key", Encode.string "deleteFeed" ), ( "value", Encode.string f.url ) ]) ] [ text "Delete" ]
                                    ]
                                ]
                        )
                        feeds
                , div
                    [ class
                        (if List.length feeds == 0 then
                            "opacity-60"

                         else
                            "hidden"
                        )
                    ]
                    [ div [] [ text "You don't have any feeds. Try adding a feed or importing from an OPML file?" ]
                    , div []
                        [ text "Or you can "
                        , span
                            [ class "cursor-pointer text-blue-600 opacity-100"
                            , onClick <|
                                ShowPrompt "Enter feed URL:"
                                    (Encode.object
                                        [ ( "key", Encode.string "addFeed" )
                                        , ( "defaultValue", Encode.string "https://notes.druchan.com/feed.xml" )
                                        ]
                                    )
                            ]
                            [ text "click here" ]
                        , text " to add a sample feed real quick."
                        ]
                    ]
                , span
                    [ class <|
                        String.join " "
                            [ "text-blue-600 cursor-pointer"
                            , if p.limit >= total then
                                "hidden"

                              else
                                ""
                            ]
                    , onClick <| GetFeeds { limit = p.limit + 100 }
                    ]
                    [ text "Load more..."
                    ]
                ]


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
        [ div [ id "app", class "flex flex-col gap-y-4" ]
            [ h1 [] [ a [ href "/" ] [ text "rdigest — dashboard" ] ]
            , div [ class "flex gap-x-3" ]
                [ a [ href "/digests" ] [ text "View digests" ]
                , span [] [ text <| "•" ]
                , a [ href "/feeds" ] [ text "View/manage feeds" ]
                , span [] [ text <| "•" ]
                , span [ onClick RefreshFeed, class "text-green-500 cursor-pointer" ] [ text "Refresh feeds" ]
                ]
            , hr [ class "border-0 h-[1px] bg-slate-400" ] []
            , div []
                [ case model.currPage of
                    DashboardPage ->
                        viewLatestDigest model.digestSearchTerm model.latestDigest

                    FeedsPage ->
                        div [] [ viewFeedsList model ]

                    DigestsPage d ->
                        case d of
                            Nothing ->
                                viewDigestsList model

                            Just _ ->
                                viewLatestDigest model.digestSearchTerm model.latestDigest
                ]
            ]
        ]
    }



-- UTILS/HELPERS


triggerFileUpload : Cmd Msg
triggerFileUpload =
    File.Select.file [] UploadedOpmlFile


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

        Loaded_ _ a _ ->
            a


pageCmdsToRun : Model -> Page -> Cmd Msg
pageCmdsToRun model page =
    case page of
        DashboardPage ->
            Cmd.batch [ getDigestForDate model Nothing ]

        FeedsPage ->
            Cmd.batch [ getFeedsList model { limit = Basics.clamp 100 10000 (getPageParams model.digests).limit } ]

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
            String.join "" [ date, " ", toMonthString month, ", ", year ]

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
            u.host ++ u.path ++ Maybe.withDefault "" (Maybe.map ((++) "?") u.query) ++ Maybe.withDefault "" (Maybe.map ((++) "#") u.fragment)


httpErrorToString : Http.Error -> String
httpErrorToString err =
    case err of
        Http.BadBody str ->
            str

        Http.BadStatus int ->
            case int of
                404 ->
                    "Nothing found."

                _ ->
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


paramsDecoder : Decode.Decoder { limit : Int, offset : Int }
paramsDecoder =
    let
        limit =
            Decode.field "limit" (Decode.maybe Decode.int)
                |> Decode.andThen
                    (\v ->
                        case v of
                            Nothing ->
                                Decode.succeed 10

                            Just a ->
                                Decode.succeed a
                    )

        offset : Decode.Decoder Int
        offset =
            Decode.field "offset" (Decode.maybe Decode.int)
                |> Decode.andThen
                    (\v ->
                        case v of
                            Nothing ->
                                Decode.succeed 0

                            Just a ->
                                Decode.succeed a
                    )
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


deleteFeed : Model -> String -> Cmd Msg
deleteFeed model feedUrl =
    Http.post
        { url = model.apiEndpoint
        , body = Http.jsonBody (makePostBody "feeds" "remove" (Encode.string feedUrl |> Just))
        , expect =
            Http.expectStringResponse
                (\res ->
                    case res of
                        Err e ->
                            ShowAlert e

                        Ok _ ->
                            GetFeeds (getPageParams model.feeds)
                )
                (handleRawHttpResponse (Decode.succeed ()))
        }


handleRawHttpResponse : Decode.Decoder a -> Http.Response String -> Result String a
handleRawHttpResponse decoder response =
    let
        errBodyDecoder =
            Decode.map2 (\a b -> ( a, b )) (Decode.at [ "error", "type" ] Decode.string) (Decode.at [ "error", "message" ] Decode.string)
    in
    case response of
        Http.GoodStatus_ metadata body ->
            let
                b =
                    if String.trim body == "" then
                        "{}"

                    else
                        body
            in
            case Decode.decodeString decoder b of
                Ok v ->
                    Ok v

                Err e ->
                    Err (Decode.errorToString e)

        Http.BadStatus_ metadata body ->
            if metadata.statusCode >= 400 && metadata.statusCode < 500 then
                case Decode.decodeString errBodyDecoder body of
                    Ok ( type_, msg_ ) ->
                        Err <| ([ type_, msg_ ] |> String.join ": ")

                    Err _ ->
                        Err ([ "I got an error status for that:", fromInt metadata.statusCode, metadata.statusText ] |> String.join " ")

            else
                Err ([ "I got an error status for that:", fromInt metadata.statusCode, metadata.statusText ] |> String.join " ")

        Http.BadUrl_ string ->
            Err ([ "I think the requested URI is invalid or malformed.", "We tried to call:", string ] |> String.join " ")

        Http.NetworkError_ ->
            Err "Uh oh, there was some network issue. Try again?"

        Http.Timeout_ ->
            Err "The server has taken too long to process this."


addFeed : Model -> String -> Cmd Msg
addFeed model url =
    Http.post
        { url = model.apiEndpoint
        , body = Http.jsonBody (makePostBody "feeds" "add" (Encode.object [ ( "urls", Encode.list Encode.string [ url ] ) ] |> Just))
        , expect = Http.expectStringResponse AddFeedResult (handleRawHttpResponse (Decode.succeed ()))
        }


refreshFeed : Model -> Cmd Msg
refreshFeed model =
    Http.post
        { url = model.apiEndpoint
        , body = Http.jsonBody (makePostBody "feeds" "refresh" Nothing)
        , expect = Http.expectWhatever (\_ -> NoOp)
        }


getFeedsList : Model -> PageParams -> Cmd Msg
getFeedsList model p =
    Http.post
        { url = model.apiEndpoint
        , body = Http.jsonBody (makePostBody "feeds" "list" <| Just (encodePageParams p))
        , expect =
            Http.expectJson
                (\res ->
                    case res of
                        Err e ->
                            GotFeedsList (Err <| httpErrorToString e)

                        Ok f ->
                            GotFeedsList (Ok f)
                )
                (Decode.map3
                    (\a b c -> ( a, b, c ))
                    (Decode.field "feeds" feedListDecoder)
                    (Decode.succeed p)
                    (Decode.field "total" Decode.int)
                )
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
                            case e of
                                Http.BadStatus 404 ->
                                    GotDigests (Err "No digest here. Maybe try adding feeds / refreshing feeds?")

                                _ ->
                                    GotDigests (Err <| httpErrorToString e)

                        Ok f ->
                            GotDigests (Ok f)
                )
                (Decode.map3 (\a b c -> ( a, b, c )) (Decode.field "digests" <| Decode.list Decode.string) (Decode.succeed pageParams) (Decode.field "total" Decode.int))
        }



-- PORTS


port showAlert : String -> Cmd msg


port showPrompt : ( String, Encode.Value ) -> Cmd msg


port showConfirm : ( String, Encode.Value ) -> Cmd msg


port confirmRemoveFeed : (Bool -> msg) -> Sub msg


port getPromptResponse : (( String, String ) -> msg) -> Sub msg


port getConfirmResponse : (( String, Encode.Value ) -> msg) -> Sub msg
