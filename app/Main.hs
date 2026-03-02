{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Data.List (intercalate)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Pool (withResource)
import Data.Version (showVersion)
import Database.SQLite.Simple
import Paths_rdigest (version)
import Server (startServer)
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)
import Text.StringLike (StringLike (toString), fromString)
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
    Init -> runAppM $ do
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
            Config{..} <- ask
            res <- insertFeed' _url
            liftIO $ unless (null res) $ do
              putStrLn $ "I have added the feed: " ++ link ++ "."
            liftIO $ processFeeds connPool res
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
      liftIO $ putStrLn $ intercalate "\n" (map (\(title, url, website) -> fromMaybe url title ++ " | " ++ url ++ " | " ++ fromMaybe "" website) feeds)
    ShowVersion -> putStrLn ("rdigest v" ++ showVersion version)
    ShowHelp -> putStrLn progHelp
    UpdateFeeds -> runAppM updateAllFeedsM
    UpdateFeed url -> runAppM (updateFeed url)
    StartServer port -> do
      startServer (fromMaybe 5500 port)
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
                r <- getDigestsFull conn (PageParams 1 offset)
                case r of
                  [] -> putStrLn "No digests found."
                  (Digest{..} : _) -> do
                    putStrLn $ "# Digest for date: " <> show digestDate
                    let groupedByFeed = groupByFeed digestLinks
                    mapM_
                      ( \((ftitle, _), urls) -> do
                          putStrLn $ "\n## " <> ftitle
                          mapM_ (\(title, url) -> putStrLn $ "[" <> fromMaybe url title <> "](" <> url <> ")") urls
                      )
                      groupedByFeed
            )
    UpdateApp option -> case option of
      Just "weburl" -> do
        runAppM updateWebUrlInFeedsTable
      Just _ -> putStrLn "I didnt quite understand that command. Try `rdigest help`?"
      Nothing -> putStrLn "I need a command with `update` to know what to do. eg. `rdigest update weburl`"
    ExportOpml -> do
      runAppM $ do
        Config{..} <- ask
        liftIO $ withResource connPool $ \conn -> do
          res <- getAllFeedsForOpmlExport conn
          putStrLn $ toString $ generatelOpmlFile res
    ImportOpml file -> runAppM $ do
      Config{..} <- ask
      opml <- liftIO $ failWith GeneralError $ readFile file
      _ <- liftIO $ insertFeeds connPool (catMaybes (extractXmlUrlsFromOpmlString opml))
      liftIO $ putStrLn "Done! To get the latest from the feeds, try `rdigest feeds udpate`."
    InvalidCommand str -> do
      putStrLn str

-- FUNCTIONS
progHelp :: String
progHelp =
  "Usage: rdigest <command> [args]\n\
  \Commands:\n\
  \  help                           - Show this help.\n\
  \  version                        - Show version info.\n\
  \  init                           - Initialize the database.\n\
  \  feeds add <url>                - Add a feed. <url> must be valid HTTP(S) URL.\n\
  \  feeds remove <url>             - Remove a feed and all its associated posts.\n\
  \  feeds list                     - List all feeds.\n\
  \  feeds update                   - Update/refresh all feeds.\n\
  \  feeds export                   - Export your feeds as OPML data. Prints to stdout.\n\
  \  feeds import <file_path>       - Import feeds from an OPML file.\n\
  \  digest [offset]                - Show a digest by offset (1 = latest, 2 = previous, etc.). Optional. If no offset given, 1 is assumed.\n\
  \  start [port]                   - Start the API server on the given port (optional) or on 5500 if no port given.\n"

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
      ("update" : urest) -> case urest of
        [] -> UpdateFeeds
        (u : _) -> UpdateFeed u
      ("export" : _) -> ExportOpml
      ("import" : file : _) -> ImportOpml file
      _ -> InvalidCommand "I don't recognize that command under `rdigest feeds`. Please try `rdigest help` for a list of all available commands."
    ("digest" : rest) -> case rest of
      [] -> ShowDigest (Just 1)
      (offset : _) -> ShowDigest (readMaybe offset :: Maybe Int)
    ("version" : _) -> ShowVersion
    ("--version" : _) -> ShowVersion
    ("start" : port : _) -> StartServer (readMaybe port :: Maybe Int)
    ("start" : _) -> StartServer (Just 5500)
    ("update" : rest) -> case rest of
      [] -> UpdateApp Nothing
      (h : _) -> UpdateApp (Just h)
    _ -> InvalidCommand "I don't recognize that command. Please try `rdigest help`."

userConfirmation :: String -> IO Bool
userConfirmation msg = do
  putStrLn msg
  putStr "Type 'y' and hit enter to confirm. Any other key to abort: "
  hFlush stdout
  input <- getLine
  pure $ input == "y" || input == "Y"

updateWebUrlInFeedsTable :: AppM ()
updateWebUrlInFeedsTable = do
  Config{..} <- ask
  liftIO $
    withResource
      connPool
      ( \conn -> do
          let q1 = "select url from feeds where title is null or website_url is null;"
          let q2 = "update feeds set title = ?, website_url = ? where url = ?;"
          urls <- try' (query_' conn (fromString q1) :: IO [Only String])
          case urls of
            Right urls' ->
              forM_
                urls'
                ( \url -> do
                    let u = fromOnly url
                    putStrLn ("Updating title and website_url for: " ++ u)
                    c <- fetchUrl u
                    let (title, website_url) = getTitleAndWebsiteLink u c
                    res <- try' $ execute' conn (fromString q2) (title, website_url, u)
                    either showAppError (\_ -> putStrLn "Done.") res
                )
            Left _ -> pure ()
      )

groupByFeed :: [DigestLink] -> [((String, URL), [(Maybe String, String)])]
groupByFeed xs = Map.toList $ go xs Map.empty
 where
  go :: [DigestLink] -> Map.Map (String, URL) [(Maybe String, String)] -> Map.Map (String, URL) [(Maybe String, String)]
  go [] acc = acc
  go (DigestLink{..} : rest) acc =
    go rest (Map.alter f (fromMaybe dfeedUrl dfeedTitle, dfeedUrl) acc)
   where
    f Nothing = Just [(dtitle, dlink)]
    f (Just x) = Just $ (dtitle, dlink) : x

-- TEST
