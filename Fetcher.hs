{-# LANGUAGE PartialTypeSignatures #-}
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
   ebody <- syncIO (simpleHttp url)
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
  runDb pool $ insertOrUpdateData (concat items)

loop pool refreshDelayMicros = do
  logMsg "updating feeds"
  fetchAll pool
  logMsg "done updating feeds, sleeping"
  threadDelay refreshDelayMicros
  loop pool refreshDelayMicros

startFetcher pool refreshDelayMicros = forkIO (loop pool refreshDelayMicros)
