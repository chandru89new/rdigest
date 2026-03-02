{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}

module Utils where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Lazy (ByteString)
import Data.CaseInsensitive
import qualified Data.Map as Map
import Data.Maybe
import Data.Pool (defaultPoolConfig, destroyAllResources, newPool)
import Data.String (IsString (fromString))
import Data.Text
import Data.Text.Encoding
import Data.Time
import Database.SQLite.Simple
import Network.HTTP.Simple hiding (Query)
import Network.URI hiding (query)
import System.Environment
import Text.HTML.TagSoup
import Text.XML
import Types

getDBFile :: String -> String
getDBFile = (++ "/rdigest.db")

fetchUrl :: String -> IO BS.ByteString
fetchUrl url' = failWith FetchError $ do
  req <- parseRequest url'
  let withHeader = setRequestHeader (mk $ BS.pack "User-Agent") [BS.pack "Mozilla/5.0"] req
  httpBS withHeader >>= pure . getResponseBody

failWith :: (String -> AppError) -> IO a -> IO a
failWith mkError action = do
  res <- try action
  case res of
    Left (e :: SomeException) -> (throw . mkError . show) e
    Right v -> pure v

failWith' :: (String -> AppError) -> IO a -> IO (Either AppError a)
failWith' mkError action = do
  res <- try action
  pure $ case res of
    Left (e :: SomeException) -> Left $ (mkError . show) e
    Right v -> Right v

extractTitleFromFeedUrl :: URL -> BS.ByteString -> String
extractTitleFromFeedUrl url' contents =
  let tags = parseTags (decodeUtf8 contents)
   in case getInnerText $ takeBetween "<title>" "</title>" tags of
        "" -> url'
        x -> x

extractLinkFromFeedYtUrl :: URL -> BS.ByteString -> String
extractLinkFromFeedYtUrl url' contents =
  let tags = parseTags (decodeUtf8 contents)
   in case getInnerText $ takeBetween "<uri>" "</uri>" tags of
        "" -> url'
        x -> x

extractLinkFromFeedUrl :: URL -> BS.ByteString -> String
extractLinkFromFeedUrl url' contents =
  let tags = parseTags (decodeUtf8 contents)
   in case getInnerText $ takeBetween "<link>" "</link>" tags of
        "" -> url'
        x -> x

getInnerText :: [Tag Text] -> String
getInnerText = unpack . strip . innerText

trim :: String -> String
trim = unpack . strip . pack

takeBetween :: String -> String -> [Tag Text] -> [Tag Text]
takeBetween start end tags = Prelude.takeWhile (~/= end) $ Prelude.dropWhile (~/= start) tags

try' :: IO a -> IO (Either AppError a)
try' = (try :: IO a -> IO (Either AppError a))

query' :: (ToRow a, FromRow r) => Connection -> Query -> a -> IO [r]
query' conn q = failWith DatabaseError . query conn q

query_' :: (FromRow r) => Connection -> Query -> IO [r]
query_' conn = failWith DatabaseError . query_ conn

execute' :: (ToRow a) => Connection -> Query -> a -> IO ()
execute' conn q = failWith DatabaseError . execute conn q

execute_' :: Connection -> Query -> IO ()
execute_' conn = failWith DatabaseError . execute_ conn

runAppM :: AppM a -> IO ()
runAppM app = do
  rdigestPath' <- lookupEnv "RDIGEST_FOLDER"
  channelId <- lookupEnv "TG_CHANNEL_ID"
  chatId <- lookupEnv "TG_CHAT_ID" -- backwards compatibility
  chanId <- lookupEnv "TG_CHAN_ID" -- backwards compatibility
  let _channelId = channelId <|> chatId <|> chanId
  case rdigestPath' of
    Nothing -> showAppError $ GeneralError "It looks like you have not set the RDIGEST_FOLDER env. `export RDIGEST_FOLDER=<full-path-where-rdigest-should-save-data>"
    Just rdPath -> do
      pool <- newPool (defaultPoolConfig (open (getDBFile rdPath)) close 60.0 10)
      let config = Config{connPool = pool, template = "", rdigestPath = rdPath, indexTemplate = ""}
      res <- try' $ runReaderT app config
      destroyAllResources pool
      either showAppError (const $ return ()) res

extractFeedItems :: BS.ByteString -> Maybe [FeedItem]
extractFeedItems = parseContents
 where
  parseContents c =
    let tags = parseTags (decodeUtf8 c)
        entryTags = partitions (~== ("<entry>" :: String)) tags -- this is for youtube feeds only
        itemTags = partitions (~== ("<item>" :: String)) tags
     in case (itemTags, entryTags) of
          ([], []) -> Nothing
          (xs, []) -> Just $ Prelude.map extractFeedItem xs
          ([], ys) -> Just $ Prelude.map extractFeedItem ys
          (xs, ys) -> Just $ Prelude.map extractFeedItem (xs ++ ys)

extractFeedItem :: [Tag Text] -> FeedItem
extractFeedItem tags =
  let title' = getInnerText $ takeBetween "<title>" "</title>" tags
      linkFromYtFeed = extractLinkHref tags -- youtube specific
      link' = nothingIfEmpty . getInnerText $ takeBetween "<link>" "</link>" tags
      pubDate = nothingIfEmpty . getInnerText $ takeBetween "<pubDate>" "</pubDate>" tags
      publishedDate = nothingIfEmpty . getInnerText $ takeBetween "<published>" "</published>" tags
      updatedDate = nothingIfEmpty . getInnerText $ takeBetween "<updated>" "</updated>" tags
      updated' = pubDate <|> publishedDate <|> updatedDate
   in FeedItem{title = title', link = link' <|> linkFromYtFeed, updated = updated' >>= parseDate}

extractLinkHref :: [Tag Text] -> Maybe String
extractLinkHref tags =
  let links = extractBetweenTag "link" tags
   in case links of
        (h : _) -> Just $ unpack $ fromAttrib "href" h
        _ -> Nothing

extractBetweenTag :: Text -> [Tag Text] -> [Tag Text]
extractBetweenTag tag tags =
  let startTag = TagOpen tag []
      endTag = TagClose tag
   in Prelude.takeWhile (~/= endTag) $ Prelude.dropWhile (~/= startTag) tags

extractXmlUrl :: Tag Text -> (Maybe String)
extractXmlUrl tag =
  let xs = extractBetweenTag "outline" [tag]
   in case xs of
        (h : _) -> (Just $ unpack $ fromAttrib "xmlUrl" h)
        _ -> (Nothing)

extractXmlUrlsFromOpmlString :: String -> [Maybe String]
extractXmlUrlsFromOpmlString c =
  let tags = Prelude.filter (isTagOpenName "outline") (parseTags (pack c))
   in Prelude.map extractXmlUrl tags

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = Prelude.take n xs : Utils.chunksOf n (Prelude.drop n xs)

toMicroseconds :: Int -> Int
toMicroseconds x = x * 1000 * 1000

justRunQuery :: Connection -> Query -> IO ()
justRunQuery conn q = do
  _ <- execute_' conn q
  pure ()

parseDate :: String -> Maybe Day
parseDate datetime = fmap utctDay $ firstJust $ Prelude.map tryParse [fmt1, fmt2, fmt3, fmt4, fmt5, fmt6]
 where
  fmt1 = "%Y-%m-%dT%H:%M:%S%z"
  fmt2 = "%a, %d %b %Y %H:%M:%S %z"
  fmt3 = "%a, %d %b %Y %H:%M:%S %Z"
  fmt4 = "%Y-%m-%dT%H:%M:%S%Z"
  fmt5 = "%Y-%m-%dT%H:%M:%S%Q%z"
  fmt6 = "%Y-%m-%dT%H:%M:%S%Q%Z"
  tryParse fmt = parseTimeM True defaultTimeLocale fmt datetime :: Maybe UTCTime
  firstJust :: [Maybe a] -> Maybe a
  firstJust xs = go xs Nothing
   where
    go [] acc = acc
    go (x : xs_) acc = case x of
      Just y -> Just y
      Nothing -> go xs_ acc

replaceContent :: String -> String -> String -> String
replaceContent pattern replaceWith content = unpack $ replace (pack pattern) (pack replaceWith) (pack content)

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %d, %Y"

nothingIfEmpty :: (Foldable t) => t a -> Maybe (t a)
nothingIfEmpty a = if Prelude.null a then Nothing else Just a

parseURL :: String -> Maybe URL
parseURL url' = case parseURI url' of
  Just uri -> (if uriScheme uri `Prelude.elem` ["http:", "https:"] then Just url' else Nothing)
  Nothing -> Nothing

extractRssLinkTag :: [Tag Text] -> [Tag Text]
extractRssLinkTag = Prelude.filter (\t -> isRssLink t || isAtomLink t)
 where
  isRssLink (TagOpen "link" attrs) = fromAttrib "type" (TagOpen "link" attrs) == "application/rss+xml"
  isRssLink _ = False
  isAtomLink (TagOpen "link" attrs) = fromAttrib "type" (TagOpen "link" attrs) == "application/atom+xml"
  isAtomLink _ = False

getFeedUrlFromWebsite :: String -> IO (Maybe String)
getFeedUrlFromWebsite url' = do
  putStrLn $ "Getting contents of: " <> url'
  contents <- fetchUrl url' >>= pure . decodeUtf8
  let tags = extractRssLinkTag $ parseTags contents
  pure $ case tags of
    tag : _ -> Just $ unpack $ fromAttrib "href" tag
    _ -> Nothing

getYtRssFeeds :: [String] -> IO [String]
getYtRssFeeds urls = do
  mapM getFeedUrlFromWebsite urls >>= pure . catMaybes

-- findValidFeedUrl :: String -> IO (Maybe String)
-- findValidFeedUrl url = do
--   let url' = dropTrailingSlash url
--   let b = url' <> "/feed.xml"
--   let c = url' <> "/atom.xml"
--   let d = url' <> "/feed"
--   let e = url' <> "/rss"
--   a <- getFeedUrlFromWebsite url'
--   pure Nothing

-- dropTrailingSlash :: String -> String
-- dropTrailingSlash str = if (Prelude.null str && (Prelude.last str == '/')) then Prelude.init str else str

getTitleAndWebsiteLink :: URL -> BS.ByteString -> (String, String)
getTitleAndWebsiteLink url' contents = do
  let title' = extractTitleFromFeedUrl url' contents
  let website = if "youtube.com/feeds/videos.xml" `isInfixOf` (pack url') then extractLinkFromFeedYtUrl url' contents else extractLinkFromFeedUrl url' contents
  (title', website)

makeOpmlOutlineItem :: (URL, Maybe String) -> String
makeOpmlOutlineItem (url', title') =
  "<outline " ++ Prelude.unwords attrs ++ " />"
 where
  attrs = [toAttrib "title" (fromMaybe "" title'), toAttrib "text" (fromMaybe "" title'), toAttrib "xmlUrl" url', toAttrib "type" "rss", toAttrib "version" "RSS"]
  toAttrib attrib val = attrib ++ "=" ++ "\"" ++ val ++ "\""

generatelOpmlFile :: [(URL, Maybe String)] -> ByteString
generatelOpmlFile urls =
  renderLBS def (Document prologue root [])
 where
  prologue = Prologue [] Nothing []
  root = Element "opml" (Map.singleton "version" "2.0") [NodeElement $ Element "body" Map.empty items]
  items = Prelude.map (\(url', title') -> NodeElement $ Element "outline" (Map.fromList [("xmlUrl", fromString url'), ("title", fromString (fromMaybe "" title')), ("text", fromString (fromMaybe "" title'))]) []) urls