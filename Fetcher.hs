module Fetcher (
  startFetcher
) where

import Configuration
import qualified Text.Feed.Types as F
import qualified Text.Feed.Import as F
import Feeds (feedToData, insertOrUpdateData)
import Network.HTTP (mkRequest, simpleHTTP, RequestMethod(GET), rspBody, Request, Response)
import Network.Stream (Result)
import Control.Concurrent
import System.IO
import Data.Time.Clock(getCurrentTime)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B
import Network.URI (parseURI)
import Control.Exception.Base (catch, SomeException)

-- This is the same function as the one defined in Network.HTTP except it
-- returns a bytestring
getRequest
    :: String               -- URL to fetch
    -> Request B.ByteString -- The constructed request
getRequest urlString =
  case parseURI urlString of
    Nothing -> error ("getRequest: Not a valid URL - " ++ urlString)
    Just u  -> mkRequest GET u

-- fetches and parses a feed
fetchFeed :: String -> IO (Maybe F.Feed)
fetchFeed url = do
  eresp <- simpleHTTP (getRequest url)
  return $ case eresp of
    Left _ -> Nothing
    Right resp -> F.parseFeedString (toString (rspBody resp))


fetchAndInsert :: Configuration -> String -> IO ()
fetchAndInsert config url = do
  mfeed <- fetchFeed url
  case mfeed of
    Nothing -> print ("failed to fetch " ++ url)
    Just feed -> do
      now <- getCurrentTime
      uncurry (insertOrUpdateData config) (feedToData url now feed) `catch` log
  where log :: SomeException -> IO ()
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

startFetcher config = forkIO (loop config)
