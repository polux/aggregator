{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Fetcher (
  startFetcher
) where

import Configuration
import qualified Text.Feed.Types as F
import qualified Text.Feed.Import as F
import qualified Data as D
import Feeds (feedToData, insertOrUpdateData, getAllFeedUrls)
import Network.HTTP.Conduit (newManager, tlsManagerSettings, parseUrlThrow, requestHeaders, httpLbs, responseBody)
import Control.Concurrent
import System.IO
import Data.Time.Clock(getCurrentTime)
import Data.ByteString.Lazy.UTF8 (toString)
import qualified Data.ByteString.Lazy as B
import Network.URI (parseURI)
import Control.Monad.Except
import Control.Monad.Reader (ReaderT)
import UnexceptionalIO (syncIO)
import Database.Persist.Sql (runSqlPool, ConnectionPool, SqlBackend)
import Control.Concurrent.Async (mapConcurrently, async)

logMsg str = do
  now <- getCurrentTime
  putStrLn ("[" ++ show now ++ "] " ++ str)
  hFlush stdout

runDb :: ConnectionPool -> ReaderT SqlBackend IO a -> IO a
runDb pool action = runSqlPool action pool

fetch :: ConnectionPool -> D.FeedId -> String -> IO [D.Item]
fetch pool feedId url = do
   logMsg ("fetching " ++ url)
   ebody <- syncIO $ do
     man <- newManager tlsManagerSettings
     req <- liftIO $ parseUrlThrow url
     let req' = req { requestHeaders = ("User-Agent", "reader@1.0 by /u/polux2001") : ("Connection", "close") : requestHeaders req }
     responseBody <$> httpLbs req' man
   case ebody of
     Left e -> do
       logMsg ("failed to fetch " ++ url ++ ": " ++ show e)
       return []
     Right body ->
       case F.parseFeedString (toString body) of
         Nothing -> do
           logMsg ("failed to parse " ++ url)
           return []
         Just feed -> do
           now <- getCurrentTime
           logMsg ("done fetching " ++ url)
           return (feedToData url now feedId feed)

fetchAll pool = do
  urls <- runDb pool $ getAllFeedUrls
  items <- mapConcurrently (uncurry (fetch pool)) urls
  -- We can't have each 'fetch' write to the database because SQLite doesn't
  -- support concurrent modifications. So we fetch everything in parallel and
  -- then make one big write.
  logMsg "done fetching feeds, writing everything to database"
  -- Even though sometimes writing to sqlite throws an error because the
  -- filesystem lock hasn't been released yet (I don't who holds it). The right
  -- way of avoiding this would be to protect writes to the pool with an mvar
  -- acting as a lock. For now we use something cheaper: we catch the IO error
  -- and retry up to three times.
  tryManyTimes 3 $ runDb pool $ insertOrUpdateData (concat items)
  logMsg "done writing everything to database"

tryManyTimes 0 _ = logMsg "tried to many times, giving up"
tryManyTimes n action = do
  eresult <- syncIO action
  case eresult of
    Left e -> do
      logMsg (show e)
      logMsg "retrying"
      tryManyTimes (n-1) action
    Right result ->
      return result

loop pool refreshDelayMicros = do
  logMsg "updating feeds"
  fetchAll pool
  logMsg "done updating feeds, sleeping"
  threadDelay refreshDelayMicros
  loop pool refreshDelayMicros

startFetcher pool refreshDelayMicros = forkIO (loop pool refreshDelayMicros)
