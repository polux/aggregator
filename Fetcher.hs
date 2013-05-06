module Fetcher (
  startFetcher
) where

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


fetchAndInsert :: String -> IO ()
fetchAndInsert url = do
  mfeed <- fetchFeed url
  case mfeed of
    Nothing -> print ("failed to fetch " ++ url)
    Just feed -> do
      now <- getCurrentTime
      uncurry insertOrUpdateData (feedToData url now feed)

slashdot = "http://rss.slashdot.org/Slashdot/slashdot"
redditHaskell = "http://www.reddit.com/r/haskell/.rss"
dartNews = "http://news.dartlang.org/feeds/posts/default"

fetchAll = do
  fetchAndInsert slashdot
  fetchAndInsert redditHaskell
  fetchAndInsert dartNews

logMsg str = do
  now <- getCurrentTime
  putStrLn ("[" ++ show now ++ "] " ++ str)
  hFlush stdout

loop = do
  logMsg "updating feeds"
  fetchAll
  logMsg "done, sleeping"
  threadDelay (10 * 60 * 1000 * 1000)
  loop

startFetcher = forkIO loop
