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

module Server where

import Control.Exception (try)
import Control.Monad.IO.Class
import DB
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.Pool
import Data.Text
import Database.SQLite.Simple
import Network.HTTP.Types (status204, status400, status404)
import System.Environment (lookupEnv)
import Types
import Utils
import Web.Scotty (ActionM, finish, json, jsonData, post, scotty, status)

-- TYPES

-- MAIN

startServer :: Int -> IO ()
startServer port = do
  putStrLn $ "Starting server at " <> show port
  rdPath <- (lookupEnv "RDIGEST_PATH") >>= pure . fromMaybe ""
  pool <- newPool (defaultPoolConfig (open (getDBFile rdPath)) close 60.0 10)
  scotty port $ do
    post "/api/v1" $ do
      r@RpcRequest{..} <- jsonData :: ActionM RpcRequest
      case (reqResource, reqAction) of
        (Health, Test) -> json $ object ["status" .= ("ok" :: Text)]
        (Feeds, Get) -> do
          (FeedId fid) <- parseRequest reqData :: ActionM FeedId
          feed <- liftIO $ try' $ withResource pool (getFeed fid)
          case feed of
            Left e -> do
              status status400
              json $ object ["error" .= appErrToObj e]
            Right [] -> do
              status status404
              json $ object ["error" .= ("Not found" :: Text)]
            Right (h : _) -> json $ toJSON h
        (Feeds, List) -> do
          pageParams <- case reqData of
            Nothing -> pure $ PageParams (Just 10) (Just 0) :: ActionM PageParams
            Just value -> parseRequest (Just value) :: ActionM PageParams
          feeds <- liftIO $ try' $ withResource pool (getFeedsListWithParams pageParams)
          case feeds of
            Left e -> do
              status status400
              json $ object ["error" .= appErrToObj e]
            Right f' -> json $ object ["feeds" .= toJSON f', "params" .= toJSON pageParams]
        (Feeds, Add) -> do
          AddFeedReq{..} <- parseRequest reqData :: ActionM AddFeedReq
          case addFeedUrls of
            (h : _) -> do
              insertResult <- liftIO $ try' $ withResource pool (insertFeed h)
              case insertResult of
                Left e -> do
                  status status400
                  json $ appErrToObj e
                Right (fid, furl) -> json $ object ["id" .= fid, "url" .= furl]
            _ -> do
              status status400
              json $ object ["error" .= ("Need at least one feed." :: Text)]
          finish
        (Feeds, Delete) -> do
          (FeedId fid) <- parseRequest reqData :: ActionM FeedId
          removeResult <- liftIO $ withResource pool (removeFeedUsingId fid)
          case removeResult of
            Right _ -> do
              status status204
              json $ object []
            Left e -> do
              status status400
              json $ appErrToObj e
        (Links, List) -> do
          pageParams <- case reqData of
            Nothing -> pure $ PageParams (Just 10) (Just 0) :: ActionM PageParams
            Just value -> parseRequest (Just value) :: ActionM PageParams
          links <- liftIO $ try' $ withResource pool (getFeedLinksWithParams pageParams)
          case links of
            Right links' -> json $ object ["links" .= toJSON links', "params" .= toJSON pageParams]
            Left e -> do
              status status400
              json $ object ["error" .= appErrToObj e]
        (Echo, _) -> json $ object ["request" .= show r]
        _ -> do
          status status400
          json $ object ["error" .= ("Invalid resource or action" :: Text), "request" .= show r]

-- HELPERS

parseRequest :: (FromJSON a) => Maybe Value -> ActionM a
parseRequest Nothing = do
  status status400
  json $ object ["error" .= ("No request data found" :: Text)]
  finish
parseRequest (Just value) = case fromJSON value of
  Success a -> pure a
  Error e -> do
    status status400
    json $ object ["error" .= (pack e :: Text)]
    finish

appErrToObj :: AppError -> ApiErrorObject
appErrToObj (GeneralError e) = ApiErrorObject "GeneralError" e
appErrToObj (FetchError e) = ApiErrorObject "FetchError" e
appErrToObj (DatabaseError e) = ApiErrorObject "DatabaseError" e
appErrToObj (FeedParseError e) = ApiErrorObject "FeedParseError" e
appErrToObj (ArgError e) = ApiErrorObject "ArgError" e
appErrToObj (DigestError e) = ApiErrorObject "DigestError" e
appErrToObj e = ApiErrorObject "Unknown" $ show e

test start limit = do
  pool <- newPool (defaultPoolConfig (open (getDBFile "/Users/chandrashekharv/Documents/rdigest-data")) close 60.0 10)
  withResource pool (getFeedsListWithParams (PageParams start limit))