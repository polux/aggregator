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

logMsg str = do
  now <- getCurrentTime
  putStrLn ("[" ++ show now ++ "] " ++ str)
  hFlush stdout

runDb :: ConnectionPool -> ReaderT SqlBackend IO a -> IO a
runDb pool action = runSqlPool action pool

fetchAndInsert :: ConnectionPool -> D.FeedId -> String -> IO ()
fetchAndInsert pool feedId url = do
   logMsg ("fetching " ++ url)
   ebody <- syncIO (simpleHttp url)
   case ebody of
     Left e -> logMsg ("failed to fetch " ++ url ++ ": " ++ show e)
     Right body ->
       case F.parseFeedString (toString body) of
         Nothing -> logMsg ("failed to parse " ++ url)
         Just feed -> do
           now <- getCurrentTime
           runDb pool $ insertOrUpdateData (feedToData url now feedId feed)

fetchAll pool = do
  urls <- runDb pool $ getAllFeedUrls
  mapM_ (uncurry (fetchAndInsert pool)) urls

loop pool refreshDelayMicros = do
  logMsg "updating feeds"
  fetchAll pool
  logMsg "done, sleeping"
  threadDelay refreshDelayMicros
  loop pool refreshDelayMicros

startFetcher pool refreshDelayMicros = forkIO (loop pool refreshDelayMicros)
