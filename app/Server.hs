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
import Network.HTTP.Types.Status
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
          runApiFn
            (errWithStatus status400)
            ( \x -> case x of
                [] -> do
                  status status404
                  json $ object ["error" .= ("Not found" :: Text)]
                (h : _) -> json $ toJSON h
            )
            $ withResource pool (getFeed fid)
        (Feeds, List) -> do
          pageParams <- case reqData of
            Nothing -> pure $ PageParams (Just 10) (Just 0) :: ActionM PageParams
            Just value -> parseRequest (Just value) :: ActionM PageParams
          runApiFn
            (errWithStatus status400)
            (\x -> json $ object ["feeds" .= toJSON x, "params" .= toJSON pageParams])
            $ withResource pool (getFeedLinksWithParams pageParams)
        (Feeds, Add) -> do
          AddFeedReq{..} <- parseRequest reqData :: ActionM AddFeedReq
          case addFeedUrls of
            (h : _) -> do
              runApiFn
                (errWithStatus status400)
                (\(fid, furl) -> json $ object ["id" .= fid, "url" .= furl])
                $ withResource pool (insertFeed h)
            _ -> do
              status status400
              json $ object ["error" .= ("Need at least one feed." :: Text)]
          finish
        (Feeds, Remove) -> do
          url <- parseRequest reqData :: ActionM String
          runApiFn
            (errWithStatus status400)
            (\_ -> status status204)
            $ withResource pool (removeFeed url)
        (Links, List) -> do
          pageParams <- case reqData of
            Nothing -> pure $ PageParams (Just 10) (Just 0) :: ActionM PageParams
            Just value -> parseRequest (Just value) :: ActionM PageParams
          runApiFn
            (errWithStatus status400)
            (\links -> json $ object ["links" .= toJSON links, "params" .= toJSON pageParams])
            $ withResource pool (getFeedLinksWithParams pageParams)
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

runApiFn :: (AppError -> ActionM ()) -> (a -> ActionM ()) -> IO a -> ActionM ()
runApiFn handleError handleSuccess action = do
  res <- liftIO $ try' action
  case res of
    Left e -> do
      handleError e
      finish
    Right r -> do
      handleSuccess r
      finish

errWithStatus :: Status -> AppError -> ActionM ()
errWithStatus s e = do
  status s
  json $ object ["error" .= appErrToObj e]

-- TEST

test start limit = do
  pool <- newPool (defaultPoolConfig (open (getDBFile "/Users/chandrashekharv/Documents/rdigest-data")) close 60.0 10)
  withResource pool (getFeedsListWithParams (PageParams start limit))