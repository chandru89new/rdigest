{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Use <&>" #-}
{-# HLINT ignore "Use <$>" #-}

module Main where

import CLI
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Reader (ask)
import DB
import qualified Data.ByteString.Char8 as BS
import Data.FileEmbed
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Pool (defaultPoolConfig, newPool, withResource)
import Data.String (IsString (fromString))
import Data.Version (showVersion)
import Database.SQLite.Simple (Connection, Only (Only, fromOnly), close, open, withTransaction)
import Paths_rdigest (version)
import Server (startServer)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Text.StringLike (StringLike (toString))
import Types
import Utils

-- MAIN

main :: IO ()
main = do
  command <- getCommand
  main' command

main' :: Command -> IO ()
main' command =
  case command of
    Init -> do
      runAppM $ do
        liftIO $ do
          putStrLn "Initializing rdigest..."
          putStrLn "If you ran this already, do not worry: all your current `rdigest` data is safe."
        initDB
        liftIO $ putStrLn "Done."
    AddFeed link -> do
      let url = parseURL link
      case url of
        Just _url -> do
          runAppM $ do
            res <- insertFeed' _url
            liftIO $ unless (null res) $ do
              putStrLn $ "I have added the feed: " ++ link ++ "."
            liftIO $ processFeeds res
        Nothing -> putStrLn "I am not able to parse the URL. Please provide a valid URL."
    RemoveFeed url -> runAppM $ do
      input <- liftIO $ userConfirmation "This will remove the feed and all the posts associated with it."
      if input
        then do
          removeFeed' url
          liftIO $ putStrLn "Removed."
        else liftIO $ putStrLn "I have cancelled it."
    ListFeeds -> runAppM $ do
      feeds <- listFeeds
      liftIO $ putStrLn $ intercalate "\n" (map (\(title, url) -> fromMaybe url title ++ " (" ++ url ++ ")") feeds)
    ShowVersion -> putStrLn ("rdigest v" ++ showVersion version)
    ShowHelp -> putStrLn progHelp
    UpdateFeeds -> runAppM updateAllFeeds
    StartServer port -> startServer (fromMaybe 5500 port)
    ShowDigest n -> do
      let offset = case n of
            Nothing -> 0
            Just x -> max x 1 - 1
      runAppM $ do
        Config{..} <- ask
        liftIO $
          withResource
            connPool
            ( \conn -> do
                r <- getDigests conn (PageParams (Just 1) (Just offset))
                case r of
                  [] -> putStrLn "No digests found."
                  (Digest{..} : _) -> do
                    putStrLn $ "## Digest for date: " <> show digestDate
                    mapM_ (\DigestLinks{..} -> putStrLn ("[" <> fromMaybe dlink dtitle <> "](" <> dlink <> ")")) digestLinks
            )
    InvalidCommand -> do
      putStrLn "I could not recognize that command. Try `rdigest help`."

-- FUNCTIONS

progHelp :: String
progHelp =
  "Usage: rdigest <command> [args]\n\
  \Commands:\n\
  \  help - Show this help.\n\
  \  version - Show version info.\n\
  \  add <feed_url> - Add a feed. <feed_url> must be valid HTTP(S) URL.\n\
  \  remove <feed_url> - Remove a feed and all its associated posts with the given url.\n\
  \  digest - Generate/update all daily digests.\n\
  \  digest --for <date> - Generate the digest for the given date. Date in the YYYY-MM-DD format.\n\
  \  digest --from <start_date> --to <end_date> - Generate digests for each day (one digest per day) in the given date range. It only generates digests for dates for which posts exists in the database. Dates in the YYYY-MM-DD format.\n\
  \  list feeds - List all feeds.\n\
  \  refresh - Refresh all feeds.\n\
  \  refresh <feed_url> - Refresh feed at <feed_url>. The <feed_url> must already be in your database.\n\
  \  purge - Purge everything.\n"

getCommand :: IO Command
getCommand = do
  args <- getArgs
  pure $ case args of
    ("help" : _) -> ShowHelp
    ("init" : _) -> Init
    ("feeds" : rest) -> case rest of
      ("add" : url : _) -> AddFeed url
      ("remove" : url : _) -> RemoveFeed url
      ("list" : _) -> ListFeeds
      ["update"] -> UpdateFeeds
      _ -> InvalidCommand
    -- ("add" : url : _) -> AddFeed url
    -- ("remove" : url : _) -> RemoveFeed url
    -- ("list" : "feeds" : _) -> ListFeeds
    -- ["refresh"] -> RefreshFeeds
    -- ["digest"] -> UpdateAllDigests
    -- ("digest" : "--for" : dayString : _) -> CreateDayDigest $ groupCommandArgs ["--for", dayString]
    -- ("digest" : xs) -> CreateRangeDigest $ groupCommandArgs xs
    -- ("purge" : _) -> PurgeEverything
    ("digest" : rest) -> case rest of
      [] -> ShowDigest (Just 1)
      (offset : _) -> ShowDigest (readMaybe offset :: Maybe Int)
    ("version" : _) -> ShowVersion
    ("--version" : _) -> ShowVersion
    ("start" : port : _) -> StartServer (readMaybe port :: Maybe Int)
    ("start" : _) -> StartServer (Just 5500)
    _ -> InvalidCommand

userConfirmation :: String -> IO Bool
userConfirmation msg = do
  putStrLn msg
  putStr "Type 'y' and hit enter to confirm. Any other key to abort: "
  hFlush stdout
  input <- getLine
  pure $ input == "y" || input == "Y"

initDB :: AppM ()
initDB = do
  (Config{..}) <- ask
  liftIO $
    withResource
      connPool
      ( \conn -> do
          applyMigrations conn
      )

-- MIGRATION LOGIC

migrations :: [(FilePath, BS.ByteString)]
migrations = $(embedDir "migrations")

getAppliedMigrationsFromTable :: Connection -> IO [FilePath]
getAppliedMigrationsFromTable conn = do
  applied <- try' $ query_' conn (fromString "select id from migrations;")
  case applied of
    Left _ -> pure []
    Right [] -> pure []
    Right a -> pure $ map fromOnly a

applyMigrations :: Connection -> IO ()
applyMigrations conn = do
  let migrationFiles = migrations
  applied <- getAppliedMigrationsFromTable conn
  let toApply = filter (\(file, _) -> file `notElem` applied) migrationFiles
  forM_
    toApply
    ( \(f, sql) -> do
        withTransaction conn $ do
          dontApplyMigration002 <- feedTableHasTitleAlready conn -- this is just in case someone has already got the updated rdigest.
          if f == "002_update_columns.sql" && dontApplyMigration002
            then execute' conn (fromString "insert into migrations (id) values (?)") $ Only f
            else do
              let queriesToRun = filter (not . null . trim) $ map toString $ BS.split ';' sql
              forM_
                queriesToRun
                ( \q -> do
                    execute_' conn (fromString q)
                )
              execute' conn (fromString "insert into migrations (id) values (?)") $ Only f
    )

feedTableHasTitleAlready :: Connection -> IO Bool
feedTableHasTitleAlready conn = do
  res <- query_' conn (fromString "select count(*) from pragma_table_info('feeds') where name = 'title'") :: IO [Only Int]
  case res of
    [] -> pure False
    (h : _) -> pure $ h > Only 0

-- TEST

test1 = do
  pool <- newPool (defaultPoolConfig (open (getDBFile "/Users/chandrashekharv/Documents/projects/rdigest")) close 60.0 10)
  withResource pool $ \conn -> do
    applyMigrations conn

test2 url = do
  r <- getYtRssFeeds [url]
  print r

test3 = do
  runAppM $ do
    Config{..} <- ask
    liftIO $
      withResource
        connPool
        ( \conn -> do
            res <- getDigests conn (PageParams (Just 2) (Just 0))
            print res
        )