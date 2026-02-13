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

import Data.Aeson
import Data.Text
import Network.HTTP.Types (status400)
import Types
import Web.Scotty (ActionM, finish, json, jsonData, post, scotty, status)

-- TYPES

-- MAIN

startServer :: Int -> IO ()
startServer port = do
  putStrLn $ "Starting server at " <> show port
  scotty port $ do
    post "/api/v1" $ do
      r@RpcRequest{..} <- jsonData :: ActionM RpcRequest
      case (reqResource, reqAction) of
        (Health, Test) -> json $ object ["status" .= ("ok" :: Text)]
        (Feeds, Add) -> do
          AddFeedReq{..} <- parseRequest reqData :: ActionM AddFeedReq
          json $ object ["urls" .= (Prelude.map pack addFeedUrls)]
          finish
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