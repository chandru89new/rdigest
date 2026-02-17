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

module Server where

import Control.Concurrent
import Control.Exception (try)
import Control.Monad (join)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import DB
import Data.Aeson
import Data.Aeson.Encoding (list)
import Data.Aeson.Types (parseField, parseFieldMaybe, parseMaybe)
import Data.ByteString hiding (isSuffixOf, pack)
import qualified Data.ByteString.Char8 as BS
import Data.FileEmbed
import Data.Maybe
import Data.Pool
import Data.Text
import Data.Text.Lazy hiding (Text, isSuffixOf, pack)
import Database.SQLite.Simple
import Network.HTTP.Types.Status
import Types
import Utils
import Web.Scotty (ActionM, captureParam, finish, get, json, jsonData, post, raw, regex, scotty, setHeader, status)

uiFiles :: [(FilePath, BS.ByteString)]
uiFiles = $(embedDir "ui/public")

indexFile = lookup "index.html" uiFiles

getContentType :: FilePath -> Text
getContentType f
  | ".html" `isSuffixOf` pack f = "text/html"
  | pack ".js" `isSuffixOf` pack f = pack "application/javascript"
  | pack ".css" `isSuffixOf` pack f = pack "text/css"
  | pack ".png" `isSuffixOf` pack f = pack "image/png"
  | pack ".svg" `isSuffixOf` pack f = pack "image/svg+xml"
  | otherwise = "application/octet-stream"

serveIndex :: ActionM ()
serveIndex = do
  case indexFile of
    Nothing -> status status404
    Just c -> do
      setHeader "Content-Type" ("text/html")
      raw (Data.ByteString.fromStrict c)

-- MAIN

startServer :: Int -> IO ()
startServer port = do
  putStrLn $ "Server started at: " <> show port
  runAppM $ do
    Config{..} <- ask
    -- rdPath <- (lookupEnv "RDIGEST_PATH") >>= pure . fromMaybe ""
    liftIO $ do
      pool <- newPool (defaultPoolConfig (open (getDBFile rdigestPath)) close 60.0 10)
      scotty port $ do
        get "/" serveIndex
        get (regex "^/(.+)$") $ do
          path <- captureParam "1"
          case lookup path uiFiles of
            Nothing -> serveIndex
            Just c -> do
              setHeader "Content-Type" (Data.Text.Lazy.fromStrict $ getContentType path)
              raw (Data.ByteString.fromStrict c)
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
                      json $ object ["error" .= appErrToObj (GeneralError "No feeds found.")]
                    (h : _) -> json $ toJSON h
                )
                $ withResource pool (getFeed fid)
            (Feeds, List) -> do
              pageParams <- extractPageParams reqData
              runApiFn
                (errWithStatus status400)
                (\(x, total) -> json $ object ["feeds" .= toJSON x, "params" .= toJSON pageParams, "total" .= toJSON total])
                $ withResource
                  pool
                  ( \conn -> do
                      lfr <- getFeedsListWithParams conn pageParams
                      total <- getTotalFeeds conn
                      pure (lfr, total)
                  )
            (Feeds, Add) -> do
              let urls = reqData >>= parseMaybe (withObject "" (.: "urls")) :: Maybe [String]
              case urls of
                Just (h : _) -> do
                  runApiFn
                    (errWithStatus status400)
                    (\(fid, furl) -> json $ object ["id" .= fid, "url" .= furl])
                    $ withResource pool (flip insertFeed h)
                _ -> do
                  status status400
                  json $ object ["error" .= appErrToObj (GeneralError "Need at least one url in the `urls`.")]
              finish
            (Feeds, Remove) -> do
              url <- parseRequest reqData :: ActionM String
              runApiFn
                (errWithStatus status400)
                (\_ -> status status204)
                $ withResource pool (removeFeed url)
            (Feeds, Refresh) -> do
              runApiFn
                (errWithStatus status400)
                (\_ -> status status204)
                $ withResource pool (forkIO . updateAllFeeds)
            (Feeds, Import) -> do
              opml <- parseRequest reqData :: ActionM String
              runApiFn
                (errWithStatus status400)
                (\_ -> status status204)
                -- \$ withResource pool (forkIO . updateAllFeeds)
                $ do
                  print opml
                  pure ()
            (Links, List) -> do
              pageParams <- extractPageParams reqData
              let feedId = join $ reqData >>= (parseMaybe (withObject "req" (.:? "feed_id"))) :: Maybe Int
              runApiFn
                (errWithStatus status400)
                (\links -> json $ object ["links" .= toJSON links, "params" .= toJSON pageParams])
                $ withResource pool (\c -> getFeedLinksWithParams c pageParams feedId)
            (Digests, List) -> do
              pageParams <- extractPageParams reqData
              runApiFn
                (errWithStatus status400)
                (\(x, t) -> json $ object ["digests" .= toJSON x, "total" .= t, "params" .= toJSON pageParams])
                $ withResource
                  pool
                  ( \conn -> do
                      total <- getTotalDigests conn
                      res <- getDigests conn pageParams
                      pure (res, total)
                  )
            (Digests, Get) -> do
              date <- if (isJust reqData) then parseRequest reqData else pure Nothing :: ActionM (Maybe String)
              runApiFn
                (errWithStatus status400)
                ( \v -> case v of
                    Nothing -> errWithStatus status404 (DatabaseError "No digest found.")
                    Just v' -> json $ toJSON v'
                )
                $ withResource pool (flip getDigestForDate date)
            (Echo, _) -> json $ object ["request" .= show r]
            _ -> do
              status status400
              json $ object ["error" .= appErrToObj (GeneralError "Invalid resource or action.")]

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

extractPageParams :: Maybe Value -> ActionM PageParams
extractPageParams reqData = do
  case reqData of
    Nothing -> pure $ PageParams 10 0 :: ActionM PageParams
    Just value -> parseRequest (Just value) :: ActionM PageParams

-- TEST

-- test start limit = do
--   pool <- newPool (defaultPoolConfig (open (getDBFile "/Users/chandrashekharv/Documents/rdigest-data")) close 60.0 10)
--   withResource pool (flip getFeedsListWithParams (PageParams start limit))