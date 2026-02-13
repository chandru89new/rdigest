-- {-# LANGUAGE RecordWildCards #-}
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

import Control.Exception
import qualified Data.ByteString.Char8 as BS
import Data.CaseInsensitive
import Data.Text
import Data.Text.Encoding
import Database.SQLite.Simple
import Network.HTTP.Simple hiding (Query)
import Text.HTML.TagSoup
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
  let tags = parseTags (unpack $ decodeUtf8 contents)
   in case getInnerText $ takeBetween "<title>" "</title>" tags of
        "" -> url'
        x -> x

getInnerText :: [Tag String] -> String
getInnerText = trim . innerText

trim :: String -> String
trim = unpack . strip . pack

takeBetween :: String -> String -> [Tag String] -> [Tag String]
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
