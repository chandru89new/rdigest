{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}

module DB where

import Control.Exception.Base
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.ByteString ()
import Data.Either
import qualified Data.Map as Map
import Data.Maybe
import Data.Pool
import Data.String (IsString (fromString))
import Data.Time
import Database.SQLite.Simple
import Text.Read
import Types
import Utils

getFeedsListWithParams :: Connection -> PageParams -> IO [ListFeedsResponse]
getFeedsListWithParams conn PageParams{..} = query' conn (fromString "select id, title, url from feeds order by id desc limit ? offset ?") (pageLimit, pageOffset)

getFeed :: Int -> Connection -> IO [ListFeedsResponse]
getFeed feedId conn = query' conn (fromString "select id, title, url from feeds where id = ?") (Only feedId)

getFeedLinksWithParams :: Connection -> PageParams -> IO [FeedLinksResponse]
getFeedLinksWithParams conn PageParams{..} = query' conn (fromString "select link, title, updated from feed_items order by updated desc limit ? offset ?") (pageLimit, pageOffset)

insertFeed :: URL -> Connection -> IO (Int, URL)
insertFeed feedUrl conn = do
  _ <- setPragmas conn
  let q = fromString "INSERT INTO feeds (title,url) VALUES (?,?);"
  let q' = fromString "SELECT id from feeds where url = ?;"
  res <- query' conn q' (Only feedUrl) :: IO [FeedId]
  if null res
    then do
      feedContents <- fetchUrl feedUrl
      let title = extractTitleFromFeedUrl feedUrl feedContents
      execute conn q (title, feedUrl)
      r' <- query' conn (fromString "SELECT id, url FROM feeds where url = ?;") (Only feedUrl) :: IO [(Int, URL)]
      if (null r')
        then throw (GeneralError "Something went wrong. Try again?")
        else pure $ head r'
    else
      throw (GeneralError "This feed already exists in your list.")

removeFeed :: URL -> Connection -> IO ()
removeFeed url conn = do
  _ <- setPragmas conn
  res <- query' conn (fromString "select id from feeds where url = ?") (Only url) :: IO [Only Int]
  case res of
    [] -> throw $ DatabaseError "A feed with that url does not exist. Maybe it's already removed?"
    _ -> execute' conn (fromString "DELETE from feeds where url = ?") (Only url)

-- removeFeedUsingId fid conn = do
--   res' <- query conn (fromString "SELECT id, url FROM feeds where id = ?;") (Only url) :: IO [(Int, String)]
--   if (null res')
--     then Left (DatabaseError "I could not find any such feed in the database. Maybe it's already gone?")
--     else do
--       failWith DatabaseError $ execute conn (fromString "DELETE from feeds where id = ?") (Only fid)
--   when (null res) $ Left $ DatabaseError ""
--   execute conn (fromString "DELETE FROM feeds where url = ?;") (Only url)

setPragmas :: Connection -> IO ()
setPragmas = flip execute_ (fromString "PRAGMA foreign_keys = ON;")

doesMigrationTableExist :: Connection -> IO Bool
doesMigrationTableExist = undefined

getDigestsFull :: Connection -> PageParams -> IO [Digest]
getDigestsFull conn PageParams{..} = do
  let q1 = "SELECT fi.updated, fi.link, fi.title, fi.feed_id, f.title, f.url AS feed_title FROM feed_items fi JOIN feeds f ON fi.feed_id = f.id WHERE fi.updated IN (SELECT DISTINCT updated FROM feed_items ORDER BY updated DESC LIMIT ? OFFSET ?) ORDER BY fi.updated DESC;"
  res <- query' conn (fromString q1) (pageLimit, pageOffset) :: IO [(Day, String, Maybe String, Int, Maybe String, String)]
  pure $ reverse $ convert $ Map.toList $ groupByUpdated res (Map.empty)
 where
  groupByUpdated :: [(Day, String, Maybe String, Int, Maybe String, String)] -> Map.Map Day [(String, Maybe String, Int, Maybe String, String)] -> Map.Map Day [(String, Maybe String, Int, Maybe String, String)]
  groupByUpdated rows acc = case rows of
    [] -> acc
    ((updated, link, title, feedId, feedTitle, feedUrl) : rest) ->
      let existing = fromMaybe [] $ Map.lookup updated acc
          newVal = (link, title, feedId, feedTitle, feedUrl) : existing
          newMap = Map.insert updated newVal acc
       in groupByUpdated rest newMap

  convert :: [(Day, [(String, Maybe String, Int, Maybe String, String)])] -> [Digest]
  convert = map f
   where
    f (date, xs) = Digest date (map g xs)
    g (link, title, feedId, feedTitle, feedUrl) = DigestLink link title feedId feedTitle feedUrl

getDigests :: Connection -> PageParams -> IO [Day]
getDigests conn p@PageParams{..} = do
  let q1 = "SELECT distinct updated from feed_items ORDER BY updated DESC limit ? offset ?;"
  res <- query' conn (fromString q1) (pageLimit, pageOffset) :: IO [Only Day]
  pure $ map fromOnly res

getFeedUrlsFromDB :: AppM [(Int, URL)]
getFeedUrlsFromDB = do
  Config{..} <- ask
  liftIO $ withResource connPool handleQuery
 where
  handleQuery :: Connection -> IO [(Int, URL)]
  handleQuery conn = do
    query_' conn selectUrlFromFeeds :: IO [(Int, String)]

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
    unwrappedFeedItems <- evaluate (extractFeedItems contents) >>= (pure . fromMaybe [])
    when (null unwrappedFeedItems) $ do
      putStrLn $ "I couldn't find anything on: " ++ url ++ "."
    res <- (try' $ doInserts conn unwrappedFeedItems)
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
    res <- try' $ insertFeedItem conn (feedId, feedItem) >>= evaluate
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

insertFeedItem :: Connection -> (Int, FeedItem) -> IO (Maybe FeedItem)
insertFeedItem conn (feedId, feedItem@FeedItem{..}) = do
  rows <- query' conn queryToCheckIfItemExists (Only link) :: IO [FeedItem]
  case rows of
    (_ : _) -> pure Nothing
    _ -> do
      _ <- execute' conn insertFeedQuery $ toRow (title, link, updated, feedId) :: IO ()
      pure (Just feedItem)

getDigestForDate :: Connection -> Maybe String -> IO (Maybe Digest)
getDigestForDate conn date = do
  let q = fromString "SELECT fi.updated, fi.link, fi.title, fi.feed_id, f.title, f.url AS feed_title FROM feed_items fi JOIN feeds f ON fi.feed_id = f.id WHERE fi.updated = ?;"
  let q' = fromString "SELECT fi.updated, fi.link, fi.title, fi.feed_id, f.title, f.url AS feed_title FROM feed_items fi JOIN feeds f ON fi.feed_id = f.id WHERE fi.updated in (SELECT DISTINCT updated FROM feed_items ORDER BY updated DESC LIMIT 1);"
  res <- case date of
    Just d -> query' conn q (Only d) :: IO [(Day, String, Maybe String, Int, Maybe String, String)]
    Nothing -> query_' conn q' :: IO [(Day, String, Maybe String, Int, Maybe String, String)]
  pure $
    case res of
      [] -> Nothing
      ((updated, _, _, _, _, _) : _) -> Just $ Digest updated (map f res)
 where
  f (_, link, title, feedId, feedTitle, feedUrl) = DigestLink link title feedId feedTitle feedUrl