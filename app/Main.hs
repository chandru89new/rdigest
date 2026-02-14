{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import Control.Applicative ((<|>))
import Control.Exception (evaluate, throw, try)
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ReaderT (runReaderT), ask)
import DB
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive (mk)
import Data.Either (fromLeft, fromRight, isLeft, isRight)
import Data.FileEmbed
import Data.List (intercalate, isInfixOf, isSuffixOf, sortBy)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Data.Ord (Down (Down), comparing)
import Data.Pool (defaultPoolConfig, destroyAllResources, newPool, withResource)
import Data.String (IsString (fromString))
import qualified Data.Text as T (Text, pack, replace, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (Day, UTCTime (utctDay), defaultTimeLocale, formatTime, parseTimeM)
import Data.Version (showVersion)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), Query, close, open, query_, toRow, withTransaction)
import Network.HTTP.Simple (getResponseBody, getResponseStatusCode, httpBS, parseRequest, setRequestBodyJSON, setRequestHeader, setRequestMethod)
import Network.URI (URI (uriAuthority, uriScheme), URIAuth (..), parseURI)
import Paths_rdigest (version)
import Server (startServer)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs, lookupEnv)
import System.IO (hFlush, stdout)
import Text.HTML.TagSoup (Tag (..), fromAttrib, parseTags, partitions, (~/=), (~==))
import Text.Read (readMaybe)
import Text.StringLike (StringLike (toString))
import Types
import Utils

-- TYPES

-- MAIN

main :: IO ()
main = do
  command <- getCommand
  main' command

main' :: Command -> IO ()
main' command =
  case command of
    Init -> do
      runAppM $ do
        liftIO $ do
          putStrLn "Initializing rdigest..."
          putStrLn "If you ran this already, do not worry: all your current `rdigest` data is safe."
        initDB
        liftIO $ putStrLn "Done."
    AddFeed link -> do
      let url = parseURL link
      case url of
        Just _url -> do
          runAppM $ do
            res <- insertFeed' _url
            liftIO $ unless (null res) $ do
              putStrLn $ "I have added the feed: " ++ link ++ "."
            liftIO $ processFeeds res
        Nothing -> putStrLn "I am not able to parse the URL. Please provide a valid URL."
    RemoveFeed url -> runAppM $ do
      input <- liftIO $ userConfirmation "This will remove the feed and all the posts associated with it."
      if input
        then do
          removeFeed' url
          liftIO $ putStrLn "Removed."
        else liftIO $ putStrLn "I have cancelled it."
    ListFeeds -> runAppM $ do
      feeds <- listFeeds
      liftIO $ putStrLn $ intercalate "\n" (map (\(title, url) -> fromMaybe url title ++ " (" ++ url ++ ")") feeds)
    ShowVersion -> putStrLn ("rdigest v" ++ showVersion version)
    ShowHelp -> putStrLn progHelp
    CreateDigest -> undefined
    ExportDigest -> undefined
    StartServer port -> startServer (fromMaybe 5500 port)
    InvalidCommand -> do
      putStrLn "I could not recognize that command. Try `rdigest help`."

-- FUNCTIONS

progHelp :: String
progHelp =
  "Usage: rdigest <command> [args]\n\
  \Commands:\n\
  \  help - Show this help.\n\
  \  version - Show version info.\n\
  \  add <feed_url> - Add a feed. <feed_url> must be valid HTTP(S) URL.\n\
  \  remove <feed_url> - Remove a feed and all its associated posts with the given url.\n\
  \  digest - Generate/update all daily digests.\n\
  \  digest --for <date> - Generate the digest for the given date. Date in the YYYY-MM-DD format.\n\
  \  digest --from <start_date> --to <end_date> - Generate digests for each day (one digest per day) in the given date range. It only generates digests for dates for which posts exists in the database. Dates in the YYYY-MM-DD format.\n\
  \  list feeds - List all feeds.\n\
  \  refresh - Refresh all feeds.\n\
  \  refresh <feed_url> - Refresh feed at <feed_url>. The <feed_url> must already be in your database.\n\
  \  purge - Purge everything.\n"

getCommand :: IO Command
getCommand = do
  args <- getArgs
  pure $ case args of
    ("help" : _) -> ShowHelp
    ("init" : _) -> Init
    ("feeds" : rest) -> case rest of
      ("add" : url : _) -> AddFeed url
      ("remove" : url : _) -> RemoveFeed url
      ("list" : _) -> ListFeeds
      _ -> InvalidCommand
    ("digest" : rest) -> case rest of
      ["create"] -> CreateDigest
      ["export"] -> ExportDigest
      _ -> InvalidCommand
    -- ("add" : url : _) -> AddFeed url
    -- ("remove" : url : _) -> RemoveFeed url
    -- ("list" : "feeds" : _) -> ListFeeds
    -- ("refresh" : url : _) -> RefreshFeed url
    -- ["refresh"] -> RefreshFeeds
    -- ["digest"] -> UpdateAllDigests
    -- ("digest" : "--for" : dayString : _) -> CreateDayDigest $ groupCommandArgs ["--for", dayString]
    -- ("digest" : xs) -> CreateRangeDigest $ groupCommandArgs xs
    -- ("purge" : _) -> PurgeEverything
    ("version" : _) -> ShowVersion
    ("--version" : _) -> ShowVersion
    ("start" : port : _) -> StartServer (readMaybe port :: Maybe Int)
    ("start" : _) -> StartServer (Just 5500)
    _ -> InvalidCommand

extractFeedItems :: BS.ByteString -> Maybe [FeedItem]
extractFeedItems = parseContents
 where
  parseContents c =
    let tags = parseTags (T.unpack $ decodeUtf8 c)
        entryTags = partitions (~== "<entry>") tags -- this is for youtube feeds only
        itemTags = partitions (~== "<item>") tags
     in case (itemTags, entryTags) of
          ([], []) -> Nothing
          (xs, []) -> Just $ map extractFeedItem xs
          ([], ys) -> Just $ map extractFeedItem ys
          (xs, ys) -> Just $ map extractFeedItem (xs ++ ys)

extractFeedItem :: [Tag String] -> FeedItem
extractFeedItem tags =
  let title = getInnerText $ takeBetween "<title>" "</title>" tags
      linkFromYtFeed = extractLinkHref tags -- youtube specific
      link = nothingIfEmpty . getInnerText $ takeBetween "<link>" "</link>" tags
      pubDate = nothingIfEmpty . getInnerText $ takeBetween "<pubDate>" "</pubDate>" tags
      publishedDate = nothingIfEmpty . getInnerText $ takeBetween "<published>" "</published>" tags
      updatedDate = nothingIfEmpty . getInnerText $ takeBetween "<updated>" "</updated>" tags
      updated = pubDate <|> publishedDate <|> updatedDate
   in FeedItem{title = title, link = link <|> linkFromYtFeed, updated = updated >>= parseDate}

insertFeedItem :: Connection -> (Int, FeedItem) -> IO (Maybe FeedItem)
insertFeedItem conn (feedId, feedItem@FeedItem{..}) = do
  rows <- query' conn queryToCheckIfItemExists (Only link) :: IO [FeedItem]
  case rows of
    (_ : _) -> pure Nothing
    _ -> do
      _ <- execute' conn insertFeedQuery $ toRow (title, link, updated, feedId) :: IO ()
      pure (Just feedItem)

insertFeed' :: URL -> AppM [(Int, URL)]
insertFeed' feedUrl = do
  Config{..} <- ask
  liftIO $ withResource connPool $ \conn -> do
    res <- insertFeed feedUrl conn
    pure [res]

getFeedUrlsFromDB :: AppM [(Int, URL)]
getFeedUrlsFromDB = do
  Config{..} <- ask
  liftIO $ failWith DatabaseError $ withResource connPool handleQuery
 where
  handleQuery :: Connection -> IO [(Int, URL)]
  handleQuery conn = do
    query_ conn selectUrlFromFeeds :: IO [(Int, String)]

selectUrlFromFeeds :: Query
selectUrlFromFeeds = fromString "SELECT id, url FROM feeds;"

selectAllFeeds :: Query
selectAllFeeds = fromString "SELECT title, url FROM feeds order by title asc;"

queryToCheckIfItemExists :: Query
queryToCheckIfItemExists = fromString "select link, title, updated from feed_items where link = ?;"

insertFeedQuery :: Query
insertFeedQuery = fromString "INSERT INTO feed_items (title, link, updated, feed_id) VALUES (?, ?, ?, ?);" :: Query

processFeed :: (Int, URL) -> AppM ()
processFeed (feedId, url) = do
  Config{..} <- ask
  liftIO $ putStrLn $ "Processing: " ++ url
  liftIO $ withResource connPool $ \conn -> do
    _ <- setPragmas conn
    feedIdExists <- query' conn (fromString "SELECT id FROM feeds where id = ?;") (Only feedId) :: IO [FeedId]
    when (null feedIdExists) $ throw $ DatabaseError "You have to first add this feed to your database. Try `rdigest add <url>`."
    contents <- fetchUrl url
    feedItems <- evaluate (extractFeedItems contents)
    let unwrappedFeedItems = fromMaybe [] feedItems
    when (null unwrappedFeedItems) $ do
      putStrLn $ "I couldn't find anything on: " ++ url ++ "."
    res <- (try $ failWith DatabaseError $ doInserts conn unwrappedFeedItems) :: IO (Either AppError [Int])
    when ((not . null) unwrappedFeedItems && isRight res) $ do
      putStrLn $ "Finished processing " ++ url ++ "."
      putStrLn $ "Discovered: " ++ show (length unwrappedFeedItems) ++ " posts."
      putStrLn $ "Added: " ++ show (sum $ fromRight [] res) ++ " posts (duplicates are ignored)."
    putStrLn "-----"
 where
  doInserts :: Connection -> [FeedItem] -> IO [Int]
  doInserts conn = mapM (handleInsert conn)

  handleInsert :: Connection -> FeedItem -> IO Int
  handleInsert conn feedItem = do
    res <- try $ insertFeedItem conn (feedId, feedItem) >>= evaluate :: IO (Either AppError (Maybe FeedItem))
    when (isLeft res) $ print (fromLeft (DatabaseError "I ran into an error when trying to save a feed to the database.") res)
    pure $ either (const 0) (\r -> if isJust r then 1 else 0) res

processFeeds :: [(Int, URL)] -> IO ()
processFeeds urls = do
  forM_ urls $ \url -> do
    res <- try' $ runAppM $ processFeed url
    case res of
      Left e -> showAppError e
      Right _ -> pure ()

updateAllFeeds :: AppM ()
updateAllFeeds = do
  urls <- getFeedUrlsFromDB
  liftIO $ processFeeds urls

removeFeed' :: URL -> AppM ()
removeFeed' url = do
  Config{..} <- ask
  liftIO $ withResource connPool $ \conn -> do
    _ <- setPragmas conn
    res <- query' conn (fromString "SELECT id, url FROM feeds where url = ?;") (Only url) :: IO [(Int, String)]
    when (null res) $ throw $ DatabaseError "I could not find any such feed in the database. Maybe it's already gone?"
    execute' conn (fromString "DELETE FROM feeds where url = ?;") (Only url)

listFeeds :: AppM [(Maybe String, String)]
listFeeds = do
  Config{..} <- ask
  liftIO $ withResource connPool $ \conn -> do
    query_' conn selectAllFeeds :: IO [(Maybe String, String)]

userConfirmation :: String -> IO Bool
userConfirmation msg = do
  putStrLn msg
  putStr "Type 'y' and hit enter to confirm. Any other key to abort: "
  hFlush stdout
  input <- getLine
  pure $ input == "y" || input == "Y"

createDigest :: Day -> AppM [FeedItemWithMeta]
createDigest day = do
  Config{..} <- ask
  liftIO $ withResource connPool $ \conn -> do
    xs <- query' conn q (Only day) :: IO [(String, String, String, String, Day)]
    pure $ map (\(url, feedTitle, link, title, updated) -> FeedItemWithMeta{feedItem = FeedItem{title = title, link = Just link, updated = Just updated}, feedTitle = feedTitle, feedURL = url}) xs
 where
  q = fromString "select feeds.url, feeds.title as feed_title, f.link, f.title, f.updated from feed_items f join feeds on f.feed_id = feeds.id where f.updated = ?;"

feedItemsToHtml :: [FeedItemWithMeta] -> String
feedItemsToHtml items = "<ul>" ++ concatMap (\item@FeedItemWithMeta{..} -> "<a class=\"li\" href=\"" ++ fromMaybe "" (link feedItem) ++ "\"><li>" ++ feedItemToHtmlLink item ++ "</li></a>") items ++ "</ul>"
 where
  feedItemToHtmlLink :: FeedItemWithMeta -> String
  feedItemToHtmlLink FeedItemWithMeta{..} =
    "<div class=\"title\">" ++ title feedItem ++ "</div><div class=\"domain\">" ++ getDomain (link feedItem) ++ " &bull; " ++ maybe "" showDay (updated feedItem) ++ "</div>"

writeDigest :: Day -> [FeedItemWithMeta] -> AppM String
writeDigest day items = do
  Config{..} <- ask
  let fileName = show day
      filePath = rdigestPath ++ "/digest-" ++ fileName ++ ".html"
      groupedByURL = groupByURL items
  liftIO $ failWith DigestError $ writeFile filePath (generateDigestContent template $ sort' groupedByURL)
  pure filePath
 where
  sort' =
    sortBy (\(k1, _) (k2, _) -> compare ((getDomain . Just . fst) k1) ((getDomain . Just . fst) k2))
  groupByURL :: [FeedItemWithMeta] -> [((URL, String), [FeedItemWithMeta])]
  groupByURL = go []
   where
    go :: [((URL, String), [FeedItemWithMeta])] -> [FeedItemWithMeta] -> [((URL, String), [FeedItemWithMeta])]
    go acc [] = acc
    go acc (x : xs) =
      let key = (feedURL x, feedTitle x)
       in case lookup key acc of
            Just ys -> go ((key, x : ys) : filter (\(k, _) -> k /= key) acc) xs
            Nothing -> go ((key, [x]) : acc) xs
  generateDigestContent :: String -> [((URL, String), [FeedItemWithMeta])] -> String
  generateDigestContent template xs =
    let titleReplaced = replaceDigestTitle ("Digest — " ++ wrapDate (show day) ++ ":") template
        summaryReplaced = replaceDigestSummary ("There are " ++ show (Prelude.length $ concatMap snd xs) ++ " posts.") titleReplaced
        contentReplaced = replaceDigestContent (concatMap convertGroupToHtml xs) summaryReplaced
     in contentReplaced

  wrapDate :: String -> String
  wrapDate date = "<span class=\"digest-date\">" ++ date ++ "</span>"

  convertGroupToHtml :: ((URL, String), [FeedItemWithMeta]) -> String
  convertGroupToHtml ((url, feedTitle), xs) = "<details open><summary><h2 class=\"summary\">" ++ feedTitle ++ " (" ++ getDomain (Just url) ++ ")</h2> (" ++ show (length xs) ++ ")</summary>" ++ feedItemsToHtml xs ++ "</details>"

getDomain :: Maybe String -> String
getDomain url =
  let maybeURI = url >>= parseURI >>= uriAuthority
   in maybe "" (trimWWW . uriRegName) maybeURI
 where
  trimWWW str = if "www." `isInfixOf` str then drop 4 str else str

replaceDigestContent :: String -> String -> String
replaceDigestContent = replaceContent "{digestContent}"

replaceDigestTitle :: String -> String -> String
replaceDigestTitle = replaceContent "{digestTitle}"

replaceDigestSummary :: String -> String -> String
replaceDigestSummary = replaceContent "{digestSummary}"

groupCommandArgs :: [String] -> [ArgPair]
groupCommandArgs = go []
 where
  go :: [ArgPair] -> [String] -> [ArgPair]
  go acc [] = acc
  go acc [x] = (x, ArgBool True) : acc
  go acc (x : y : rest)
    | isFlag x && not (isFlag y) = go ((x, ArgString y) : acc) rest
    | isFlag x && isFlag y = go ((x, ArgBool True) : acc) (y : rest)
    | otherwise = go acc (y : rest)
   where
    isFlag :: String -> Bool
    isFlag str = "--" `isInfixOf` str && length str > 2

extractArgString :: String -> [ArgPair] -> Maybe String
extractArgString key argPairs = lookup key argPairs >>= extractString
 where
  extractString :: ArgVal -> Maybe String
  extractString (ArgString str) = Just str
  extractString _ = Nothing

extractArgBool :: String -> [ArgPair] -> Maybe Bool
extractArgBool key argPairs = lookup key argPairs >>= extractBool
 where
  extractBool :: ArgVal -> Maybe Bool
  extractBool (ArgBool bool) = Just bool
  extractBool _ = Nothing

refreshFeed :: URL -> AppM ()
refreshFeed url = do
  Config{..} <- ask
  let q = fromString "select id, url from feeds where url = ?;"
  res <- liftIO $ withResource connPool $ \conn -> query' conn q (Only url) :: IO [(Int, String)]
  liftIO $ case res of
    [] -> throw $ DatabaseError $ "I could not find " ++ url ++ " in your list of feeds. Try `rdigest feeds list` to see your feeds and `rdigest feeds add` to add this feed."
    (feedId, _) : _ -> do
      processFeeds [(feedId, url)]

updateIndexFile :: AppM ()
updateIndexFile = do
  Config{..} <- ask
  liftIO $ do
    files <- getDirectoryContents rdigestPath
    let htmlFiles = filter (\file -> isSuffixOf ".html" file && file /= "index.html") files
        sortedHtmlFiles = sortBy (comparing Down) htmlFiles
    indexFileContents <- generateIndexFileContent indexTemplate sortedHtmlFiles
    failWith GeneralError $ writeFile (rdigestPath ++ "/index.html") indexFileContents
 where
  generateIndexFileContent :: String -> [FilePath] -> IO String
  generateIndexFileContent indexTemplate files = do
    let links = map (\file -> "<li><a href=\"./" ++ file ++ "\">" ++ file ++ "</a></li>") files
        content = "<ul>" ++ concat links ++ "</ul>"
    pure $ replaceContent "{indexContent}" content indexTemplate

initDB :: AppM ()
initDB = do
  (Config{..}) <- ask
  liftIO $
    withResource
      connPool
      ( \conn -> do
          applyMigrations conn
      )

runAppM :: AppM a -> IO ()
runAppM app = do
  let template = $(embedFile "./template.html")
      indexTemplate = $(embedFile "./index-template.html")
  rdigestPath <- lookupEnv "RDIGEST_FOLDER"
  channelId <- lookupEnv "TG_CHANNEL_ID"
  chatId <- lookupEnv "TG_CHAT_ID" -- backwards compatibility
  chanId <- lookupEnv "TG_CHAN_ID" -- backwards compatibility
  let _channelId = channelId <|> chatId <|> chanId
  case rdigestPath of
    Nothing -> showAppError $ GeneralError "It looks like you have not set the RDIGEST_FOLDER env. `export RDIGEST_FOLDER=<full-path-where-rdigest-should-save-data>"
    Just rdPath -> do
      pool <- newPool (defaultPoolConfig (open (getDBFile rdPath)) close 60.0 10)
      let config = Config{connPool = pool, template = BS.unpack template, rdigestPath = rdPath, indexTemplate = BS.unpack indexTemplate}
      res <- (try :: IO a -> IO (Either AppError a)) $ runReaderT app config
      destroyAllResources pool
      either showAppError (const $ return ()) res

postJSON :: String -> TgMsg -> IO BS.ByteString
postJSON url jsonBody = do
  req <- parseRequest url
  let request =
        setRequestMethod (BS.pack "POST") $
          setRequestHeader (mk $ BS.pack "Content-Type") [BS.pack "application/json"] $
            setRequestHeader (mk $ BS.pack "User-Agent") [BS.pack "Mozilla/5.0"] $
              setRequestBodyJSON jsonBody req
  response <- httpBS request
  let code = getResponseStatusCode response
  if code >= 300 || code < 200
    then throw $ NotifyError $ show (getResponseBody response)
    else pure (BS.pack "")

-- UTILS

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

toMicroseconds :: Int -> Int
toMicroseconds x = x * 1000 * 1000

justRunQuery :: Connection -> Query -> IO ()
justRunQuery conn q = do
  _ <- execute_' conn q
  pure ()

parseDate :: String -> Maybe Day
parseDate datetime = fmap utctDay $ firstJust $ map tryParse [fmt1, fmt2, fmt3, fmt4, fmt5, fmt6]
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

showAppError :: AppError -> IO ()
showAppError (FetchError msg) = putStrLn $ "Error fetching URL: " ++ msg
showAppError (DatabaseError msg) = putStrLn $ "Database error: " ++ msg
showAppError (FeedParseError msg) = putStrLn $ "Error parsing feed: " ++ msg
showAppError (ArgError msg) = putStrLn $ "Argument error: " ++ msg
showAppError (DigestError msg) = putStrLn $ "Digest error: " ++ msg
showAppError (NotifyError msg) = putStrLn $ "Notification error: " ++ msg
showAppError (GeneralError msg) = putStrLn $ "Error: " ++ msg

replaceSmartQuotes :: T.Text -> T.Text
replaceSmartQuotes =
  T.replace (T.pack "“") (T.pack "\"")
    . T.replace (T.pack "”") (T.pack "\"")
    . T.replace (T.pack "‘") (T.pack "'")
    . T.replace (T.pack "’") (T.pack "'")

replaceContent :: String -> String -> String -> String
replaceContent pattern replaceWith content = T.unpack $ T.replace (T.pack pattern) (T.pack replaceWith) (T.pack content)

showDay :: Day -> String
showDay = formatTime defaultTimeLocale "%B %d, %Y"

nothingIfEmpty :: (Foldable t) => t a -> Maybe (t a)
nothingIfEmpty a = if null a then Nothing else Just a

extractLinkHref :: [Tag String] -> Maybe String
extractLinkHref tags =
  let links = extractBetweenTag "link" tags
   in case links of
        (h : _) -> Just $ fromAttrib "href" h
        _ -> Nothing

extractBetweenTag :: String -> [Tag String] -> [Tag String]
extractBetweenTag tag tags =
  let startTag = TagOpen tag []
      endTag = TagClose tag
   in takeWhile (~/= endTag) $ dropWhile (~/= startTag) tags

parseURL :: String -> Maybe URL
parseURL url = case parseURI url of
  Just uri -> (if uriScheme uri `elem` ["http:", "https:"] then Just url else Nothing)
  Nothing -> Nothing

extractRssLinkTag :: [Tag String] -> [Tag String]
extractRssLinkTag = filter isRssLink
 where
  isRssLink (TagOpen "link" attrs) = fromAttrib "type" (TagOpen "link" attrs) == "application/rss+xml"
  isRssLink _ = False

getFeedUrlFromWebsite :: String -> IO (Maybe String)
getFeedUrlFromWebsite url = do
  putStrLn $ "Getting contents of: " <> url
  contents <- fetchUrl url >>= pure . (T.unpack . decodeUtf8)
  let tags = extractRssLinkTag $ parseTags contents
  pure $ case tags of
    tag : _ -> Just $ fromAttrib "href" tag
    _ -> Nothing

getYtRssFeeds :: [String] -> IO [String]
getYtRssFeeds urls = do
  mapM getFeedUrlFromWebsite urls >>= pure . catMaybes

-- MIGRATION LOGIC

migrations :: [(FilePath, BS.ByteString)]
migrations = $(embedDir "migrations")

getAppliedMigrationsFromTable :: Connection -> IO [FilePath]
getAppliedMigrationsFromTable conn = do
  applied <- try' $ query_' conn (fromString "select id from migrations;")
  case applied of
    Left _ -> pure []
    Right [] -> pure []
    Right a -> pure $ map fromOnly a

applyMigrations :: Connection -> IO ()
applyMigrations conn = do
  let migrationFiles = migrations
  applied <- getAppliedMigrationsFromTable conn
  let toApply = filter (\(file, _) -> file `notElem` applied) migrationFiles
  forM_
    toApply
    ( \(f, sql) -> do
        withTransaction conn $ do
          dontApplyMigration002 <- feedTableHasTitleAlready conn -- this is just in case someone has already got the updated rdigest.
          if f == "002_update_columns.sql" && dontApplyMigration002
            then execute' conn (fromString "insert into migrations (id) values (?)") $ Only f
            else do
              let queriesToRun = filter (not . null . trim) $ map toString $ BS.split ';' sql
              forM_
                queriesToRun
                ( \q -> do
                    execute_' conn (fromString q)
                )
              execute' conn (fromString "insert into migrations (id) values (?)") $ Only f
    )

feedTableHasTitleAlready :: Connection -> IO Bool
feedTableHasTitleAlready conn = do
  res <- query_' conn (fromString "select count(*) from pragma_table_info('feeds') where name = 'title'") :: IO [Only Int]
  case res of
    [] -> pure False
    (h : _) -> pure $ h > Only 0

-- TEST

test1 = do
  pool <- newPool (defaultPoolConfig (open (getDBFile "/Users/chandrashekharv/Documents/projects/rdigest")) close 60.0 10)
  withResource pool $ \conn -> do
    applyMigrations conn

test2 url = do
  r <- getYtRssFeeds [url]
  print r