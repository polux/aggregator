module Fetcher (
  startFetcher
) where

import Configuration
import qualified Text.Feed.Types as F
import qualified Text.Feed.Import as F
import Feeds (feedToData, insertOrUpdateData, deleteFeedsNotInConfig)
import Network.HTTP.Simple (parseRequest, httpLBS, Request, getResponseBody, getResponseStatus)
import Network.HTTP.Types.Status (statusIsSuccessful)
import Control.Concurrent
import System.IO
import Data.Time.Clock(getCurrentTime)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B
import Network.URI (parseURI)
import Control.Exception.Base (catch, SomeException)

getRequest
    :: String  -- URL to fetch
    -> Request -- The constructed request
getRequest urlString =
  case parseRequest urlString of
    Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
    Just u -> u

-- fetches and parses a feed
fetchFeed :: String -> IO (Maybe F.Feed)
fetchFeed url = do
  resp <- httpLBS (getRequest url)
  return $ if statusIsSuccessful (getResponseStatus resp)
    then F.parseFeedString (toString (getResponseBody resp))
    else Nothing

fetchAndInsert :: Configuration -> String -> IO ()
fetchAndInsert config url = fetchAndInsert' `catch` log
  where fetchAndInsert' = do
         mfeed <- fetchFeed url
         case mfeed of
           Nothing -> print ("failed to fetch " ++ url)
           Just feed -> do
             now <- getCurrentTime
             uncurry (insertOrUpdateData config) (feedToData url now feed)

        log :: SomeException -> IO ()
        log e = print ("failed to insert " ++ url ++ ": " ++ show e)

fetchAll config = mapM_ (fetchAndInsert config) (map fst $ feeds config)

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

startFetcher config = do
  deleteFeedsNotInConfig config
  forkIO (loop config)
