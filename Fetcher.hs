{-# LANGUAGE ScopedTypeVariables #-}

module Fetcher (
  startFetcher
) where

import Configuration
import qualified Text.Feed.Types as F
import qualified Text.Feed.Import as F
import qualified Data as D
import Feeds (feedToData, insertOrUpdateData, getAllFeedUrls)
import Network.HTTP.Conduit (simpleHttp)
import Control.Concurrent
import System.IO
import Data.Time.Clock(getCurrentTime)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B
import Network.URI (parseURI)
import Control.Monad.Except
import UnexceptionalIO (syncIO)

logMsg str = do
  now <- getCurrentTime
  putStrLn ("[" ++ show now ++ "] " ++ str)
  hFlush stdout

fetchAndInsert :: Configuration -> D.FeedId -> String -> IO ()
fetchAndInsert config feedId url = do
   logMsg ("fetching " ++ url)
   ebody <- syncIO (simpleHttp url)
   case ebody of
     Left e -> logMsg ("failed to fetch " ++ url ++ ": " ++ show e)
     Right body ->
       case F.parseFeedString (toString body) of
         Nothing -> logMsg ("failed to parse " ++ url)
         Just feed -> do
           now <- getCurrentTime
           insertOrUpdateData config (feedToData url now feedId feed)

fetchAll config = do
  urls <- getAllFeedUrls config
  mapM_ (uncurry (fetchAndInsert config)) urls

loop config = do
  logMsg "updating feeds"
  fetchAll config
  logMsg "done, sleeping"
  threadDelay (10 * 60 * 1000 * 1000)
  loop config

startFetcher config = forkIO (loop config)
