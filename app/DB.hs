{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Redundant bracket" #-}

module DB where

import Control.Exception (throw)
import Data.Maybe
import Data.String (IsString (fromString))
import Database.SQLite.Simple
import Types
import Utils

getFeedsListWithParams :: PageParams -> Connection -> IO [ListFeedsResponse]
getFeedsListWithParams PageParams{..} conn = failWith DatabaseError $ query conn (fromString "select id, title, url from feeds order by created_at desc limit ? offset ?") (fromMaybe 10 pageLimit, fromMaybe 0 pageOffset)

getFeed :: Int -> Connection -> IO [ListFeedsResponse]
getFeed feedId conn = failWith DatabaseError $ query conn (fromString "select id, title, url from feeds where id = ?") (Only feedId)

getFeedLinksWithParams :: PageParams -> Connection -> IO [FeedLinksResponse]
getFeedLinksWithParams PageParams{..} conn = failWith DatabaseError $ query conn (fromString "select link, title, updated from feed_items order by updated desc limit ? offset ?") (fromMaybe 10 pageLimit, fromMaybe 10 pageOffset)

insertFeed :: URL -> Connection -> IO (Int, URL)
insertFeed feedUrl conn = do
  let q = fromString "INSERT INTO feeds (title,url) VALUES (?,?);"
  let q' = fromString "SELECT id from feeds where url = ?;"
  res <- failWith DatabaseError $ query conn q' (Only feedUrl) :: IO [FeedId]
  if null res
    then do
      feedContents <- fetchUrl feedUrl
      let title = extractTitleFromFeedUrl feedUrl feedContents
      execute conn q (title, feedUrl)
      r' <- failWith DatabaseError $ query conn (fromString "SELECT id, url FROM feeds where url = ?;") (Only feedUrl) :: IO [(Int, URL)]
      if (null r')
        then throw (GeneralError "Something went wrong. Try again?")
        else pure $ head r'
    else
      throw (GeneralError "This feed already exists in your list.")

removeFeedUsingId :: Int -> Connection -> IO (Either AppError ())
removeFeedUsingId = undefined

-- removeFeedUsingId fid conn = do
--   res' <- query conn (fromString "SELECT id, url FROM feeds where id = ?;") (Only url) :: IO [(Int, String)]
--   if (null res')
--     then Left (DatabaseError "I could not find any such feed in the database. Maybe it's already gone?")
--     else do
--       failWith DatabaseError $ execute conn (fromString "DELETE from feeds where id = ?") (Only fid)
--   when (null res) $ Left $ DatabaseError ""
--   execute conn (fromString "DELETE FROM feeds where url = ?;") (Only url)