{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}

module CLI where

import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import DB
import Data.Pool
import Types

insertFeed' :: URL -> AppM [(Int, URL)]
insertFeed' feedUrl = do
  Config{..} <- ask
  liftIO $ withResource connPool $ \conn -> do
    res <- insertFeed conn feedUrl
    pure [res]

removeFeed' :: URL -> AppM ()
removeFeed' url = do
  Config{..} <- ask
  liftIO $ withResource connPool $ \conn -> do
    _ <- setPragmas conn
    removeFeed url conn

listFeeds :: AppM [(Maybe String, String)]
listFeeds = do
  Config{..} <- ask
  liftIO $ withResource connPool $ \conn -> do
    xs <- getFeedsListWithParams conn (PageParams 1000 0)
    pure $ map (\ListFeedsResponse{..} -> (lfrTitle, lfrUrl)) xs
