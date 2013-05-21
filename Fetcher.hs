module Fetcher (
  startFetcher
) where

import Configuration
import qualified Text.Feed.Types as F
import qualified Text.Feed.Import as F
import Feeds (feedToData, insertOrUpdateData)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Control.Concurrent
import System.IO
import Data.Time.Clock(getCurrentTime)

-- fetches and parses a feed
fetchFeed :: String -> IO (Maybe F.Feed)
fetchFeed url = do
  resp <- simpleHTTP (getRequest url)
  case resp of
    Left _ -> return Nothing
    _ -> F.parseFeedString `fmap` (getResponseBody resp)


fetchAndInsert :: Configuration -> String -> IO ()
fetchAndInsert config url = do
  mfeed <- fetchFeed url
  case mfeed of
    Nothing -> print ("failed to fetch " ++ url)
    Just feed -> do
      now <- getCurrentTime
      uncurry (insertOrUpdateData config) (feedToData url now feed)

fetchAll config = mapM_ (fetchAndInsert config) (feeds config)

logMsg str = do
  now <- getCurrentTime
  putStrLn ("[" ++ show now ++ "] " ++ str)
  hFlush stdout

loop config = do
  logMsg "updating feeds"
  fetchAll config
  logMsg "done, sleeping"
  threadDelay (10 * 60 * 1000 * 1000)
  loop config

startFetcher config = forkIO (loop config)
