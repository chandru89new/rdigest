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

module Types where

import Control.Exception
import Control.Monad.Trans.Reader
import Data.Aeson
import Data.Aeson.Key
import Data.Pool
import Data.Time
import Database.SQLite.Simple

type URL = String

data Command
  = Init
  | AddFeed URL
  | RefreshFeed URL
  | RefreshFeeds
  | ListFeeds
  | RemoveFeed URL
  | UpdateAllDigests
  | PurgeEverything
  | CreateDayDigest [ArgPair]
  | CreateRangeDigest [ArgPair]
  | ShowVersion
  | ShowHelp
  | StartServer (Maybe Int)
  | InvalidCommand
  deriving (Eq)

data AppError
  = FetchError String
  | DatabaseError String
  | FeedParseError String
  | ArgError String
  | DigestError String
  | GeneralError String
  | NotifyError String
  deriving (Show)

data Config = Config
  {connPool :: Pool Connection, template :: String, indexTemplate :: String, rdigestPath :: String}

data FeedItem = FeedItem {title :: String, link :: Maybe String, updated :: Maybe Day} deriving (Eq, Show)

data Feed = Feed {url :: String, name :: String} deriving (Show)

newtype FeedId = FeedId Int deriving (Show)

type App a = Config -> IO a

type AppM a = ReaderT Config IO a

data FeedItemWithMeta = FeedItemWithMeta {feedItem :: FeedItem, feedTitle :: String, feedURL :: URL} deriving (Show)

type ArgPair = (String, ArgVal)

data ArgVal = ArgBool Bool | ArgString String deriving (Eq, Show)

data TgMsg = TgMsg {chat_id :: String, text :: String, link_preview_options :: LinkPreviewOptions}

newtype LinkPreviewOptions = LinkPreviewOptions {link_url :: String}

newtype MultipleQueries = MultipleQueries String

instance Exception AppError

instance FromRow FeedItem where
  fromRow = FeedItem <$> field <*> field <*> field

instance FromRow FeedId where
  fromRow = FeedId <$> field

instance ToJSON TgMsg where
  toJSON tgMsg =
    object
      [ fromString "chat_id" .= chat_id tgMsg
      , fromString "text" .= text tgMsg
      , fromString "link_preview_options" .= object [fromString "url" .= link_url (link_preview_options tgMsg)]
      ]

data Resource = Health | Echo | Feeds | InvalidResource deriving (Show)

data Action = Test | Add | InvalidAction deriving (Show)

data RpcRequest = RpcRequest
  { reqResource :: Resource
  , reqAction :: Action
  , reqData :: Maybe Value
  }

data AddFeedReq = AddFeedReq {addFeedUrls :: [String]}

instance Show RpcRequest where
  show (RpcRequest{..}) =
    "Resource: "
      <> show reqResource
      <> "\n"
      <> "Action: "
      <> show reqAction
      <> "\n"
      <> "Data: "
      <> show reqData

instance FromJSON Resource where
  parseJSON = withText "Resource" $ \r -> case r of
    "health" -> pure Health
    "echo" -> pure Echo
    "feeds" -> pure Feeds
    _ -> pure InvalidResource

instance FromJSON Action where
  parseJSON = withText "Action" $ \r -> case r of
    "test" -> pure Test
    "add" -> pure Add
    _ -> pure InvalidAction

instance FromJSON RpcRequest where
  parseJSON =
    withObject
      "RpcRequest"
      ( \v ->
          RpcRequest <$> v .: "resource" <*> v .: "action" <*> v .:? "request"
      )

instance FromJSON AddFeedReq where
  parseJSON = withObject "AddFeedReq" (\v -> AddFeedReq <$> v .: "urls")
