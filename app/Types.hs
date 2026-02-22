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
import Data.Pool
import Data.Time
import Database.SQLite.Simple

type URL = String

data Command
  = Init
  | AddFeed URL
  | ListFeeds
  | RemoveFeed URL
  | ShowVersion
  | ShowHelp
  | StartServer (Maybe Int)
  | UpdateFeeds
  | ShowDigest (Maybe Int)
  | UpdateApp (Maybe String)
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

instance FromJSON FeedId where
  parseJSON = withObject "FeedId" $ \v -> FeedId <$> v .: "id"

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
      [ "chat_id" .= chat_id tgMsg
      , "text" .= text tgMsg
      , "link_preview_options" .= object ["url" .= link_url (link_preview_options tgMsg)]
      ]

data Resource = Health | Echo | Feeds | Links | Digests | InvalidResource deriving (Show)

data Action = Test | Add | List | Get | Remove | Refresh | Import | InvalidAction deriving (Show)

data RpcRequest = RpcRequest
  { reqResource :: Resource
  , reqAction :: Action
  , reqData :: Maybe Value
  }

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
    "links" -> pure Links
    "digests" -> pure Digests
    _ -> pure InvalidResource

instance FromJSON Action where
  parseJSON = withText "Action" $ \r -> case r of
    "test" -> pure Test
    "add" -> pure Add
    "list" -> pure List
    "get" -> pure Get
    "remove" -> pure Remove
    "refresh" -> pure Refresh
    "import" -> pure Import
    _ -> pure InvalidAction

instance FromJSON RpcRequest where
  parseJSON =
    withObject
      "RpcRequest"
      ( \v ->
          RpcRequest <$> v .: "resource" <*> v .: "action" <*> v .:? "request"
      )

data ListFeedsResponse = ListFeedsResponse
  { lfrId :: Int
  , lfrTitle :: Maybe String
  , lfrUrl :: String
  , lfrWebUrl :: Maybe String
  }
  deriving (Show)

instance FromRow ListFeedsResponse where
  fromRow = ListFeedsResponse <$> field <*> field <*> field <*> field

instance ToJSON ListFeedsResponse where
  toJSON (ListFeedsResponse{..}) =
    object
      [ "id" .= lfrId
      , "url" .= lfrUrl
      , "title" .= lfrTitle
      , "website_url" .= lfrWebUrl
      ]

data PageParams = PageParams
  { pageLimit :: Int
  , pageOffset :: Int
  }
  deriving (Show)

instance FromJSON PageParams where
  parseJSON = withObject "PageParams" $ \v ->
    PageParams <$> (v .:? "limit" .!= defaultPageLimit) <*> (v .:? "offset" .!= defaultOffset)

instance ToJSON PageParams where
  toJSON PageParams{..} =
    object
      [ "limit" .= pageLimit
      , "offset" .= pageOffset
      ]

data FeedLinksResponse = FeedLinksResponse
  { fiLink :: String
  , fiTitle :: String
  , fiUpdated :: String
  }

instance FromRow FeedLinksResponse where
  fromRow = FeedLinksResponse <$> field <*> field <*> field

instance ToJSON FeedLinksResponse where
  toJSON FeedLinksResponse{..} = object ["url" .= fiLink, "title" .= fiTitle, "updated" .= fiUpdated]

data ApiErrorObject = ApiErrorObject
  { errorType :: String
  , errorMsg :: String
  }

instance ToJSON ApiErrorObject where
  toJSON ApiErrorObject{..} =
    object
      [ "type" .= errorType
      , "message" .= errorMsg
      ]

data Digest = Digest
  { digestDate :: Day
  , digestLinks :: [DigestLink]
  }
  deriving (Show)

data DigestLink = DigestLink
  { dlink :: String
  , dtitle :: Maybe String
  , dfeedId :: Int
  , dfeedTitle :: Maybe String
  , dfeedUrl :: String
  }
  deriving (Show)

instance ToJSON DigestLink where
  toJSON DigestLink{..} = object ["link" .= dlink, "title" .= dtitle, "feed_id" .= dfeedId, "feed_title" .= dfeedTitle, "feed_url" .= dfeedUrl]

instance ToJSON Digest where
  toJSON Digest{..} = object ["date" .= digestDate, "links" .= map toJSON digestLinks]

showAppError :: AppError -> IO ()
showAppError (FetchError msg) = putStrLn $ "Error fetching URL: " ++ msg
showAppError (DatabaseError msg) = putStrLn $ "Database error: " ++ msg
showAppError (FeedParseError msg) = putStrLn $ "Error parsing feed: " ++ msg
showAppError (ArgError msg) = putStrLn $ "Argument error: " ++ msg
showAppError (DigestError msg) = putStrLn $ "Digest error: " ++ msg
showAppError (NotifyError msg) = putStrLn $ "Notification error: " ++ msg
showAppError (GeneralError msg) = putStrLn $ "Error: " ++ msg

defaultPageLimit :: Int
defaultPageLimit = 10

defaultOffset :: Int
defaultOffset = 0