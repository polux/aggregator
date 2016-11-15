{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings, ViewPatterns #-}

module Main where

import Yesod
import qualified Data as D
import qualified Feeds as F
import qualified Configuration as C
import Database.Persist.Sql (SqlBackend, ConnectionPool, runSqlPool)
import Database.Persist.Sqlite (withSqlitePool)
import Fetcher
import Safe (readMay)
import Data.Text as T
import System.Environment (getArgs)
import Network.Wai.Handler.Warp (run)
import qualified Network.Wai.Middleware.Cors as Cors
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Reader (ReaderT)

data App = App
  { configuration :: C.Configuration
  , connectionPool :: ConnectionPool
  }

mkYesod "App" [parseRoutes|
/feeds FeedsR GET POST
/feeds/#D.FeedId FeedR GET PUT DELETE
/feeds/#D.FeedId/items ItemsR GET
/feeds/#D.FeedId/readAll FeedReadAllR POST
/feeds/#D.FeedId/items/#D.ItemId ItemR GET
/feeds/#D.FeedId/items/#D.ItemId/read ItemReadR POST
/feeds/#D.FeedId/items/#D.ItemId/starred ItemStarredR POST
/search SearchR GET
|]

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend

  runDB action = do
    App _ pool <- getYesod
    runSqlPool action pool

getConfig :: Handler C.Configuration
getConfig = configuration `fmap` getYesod

decode :: Read b => Maybe Text -> Maybe b
decode mx = mx >>= readMay . T.unpack

lookupGetFlag :: Text -> Handler Bool
lookupGetFlag flagName = do
  mparam <- lookupGetParam flagName
  return $ maybe False (const True) mparam

getFeedsR :: Handler Value
getFeedsR = do
  config <- getConfig
  feeds <- runDB $ F.getAllFeeds
  returnJson feeds

postFeedsR :: Handler Value
postFeedsR = do
  inputFeed <- requireJsonBody
  config <- getConfig
  feed <- runDB $ F.createFeed inputFeed
  returnJson feed

getFeedR :: D.FeedId -> Handler Value
getFeedR feedId = do
  config <- getConfig
  mfeed <- runDB $ F.getFeed feedId
  case mfeed of
    Just feed -> returnJson feed
    Nothing -> invalidArgs [T.pack "unknown feed ID"]

putFeedR :: D.FeedId -> Handler Value
putFeedR feedId = do
  config <- getConfig
  feedInput <- requireJsonBody
  feed <- runDB $ F.updateFeed feedId feedInput
  returnJson feed

deleteFeedR :: D.FeedId -> Handler ()
deleteFeedR feedId = do
  config <- getConfig
  runDB $ F.deleteFeed feedId

getItemsR :: D.FeedId -> Handler Value
getItemsR feedId = do
  mend <- lookupGetParam "end"
  mmax <- lookupGetParam "max"
  light <- lookupGetFlag "descriptions-only"
  unread <- lookupGetFlag "unread-only"
  starred <- lookupGetFlag "starred-only"
  config <- getConfig
  items <- runDB $ F.getItems light unread starred (Left feedId)
                              (decode mend) (decode mmax)
  returnJson items

getSearchR :: Handler Value
getSearchR = do
  mq <- fmap T.unpack `fmap` lookupGetParam "q"
  mend <- lookupGetParam "end"
  mmax <- lookupGetParam "max"
  light <- lookupGetFlag "descriptions-only"
  unread <- lookupGetFlag "unread-only"
  starred <- lookupGetFlag "starred-only"
  config <- getConfig
  let q = maybe "" id mq
  items <- runDB $ F.getItems light unread starred (Right q)
                              (decode mend) (decode mmax)
  returnJson items

setBoolValueR :: (D.ItemId -> Bool -> ReaderT SqlBackend Handler ())
              -> D.FeedId
              -> D.ItemId
              -> Handler ()
setBoolValueR setter feedId itemId = do
  mvalue <- lookupPostParam "value"
  case decode mvalue of
    Just value -> do
      config <- getConfig
      runDB $ setter itemId value
    Nothing -> invalidArgs [T.pack "value"]

getItemR :: D.FeedId -> D.ItemId -> Handler Value
getItemR feedId itemId = do
  config <- getConfig
  mitem <- runDB $ F.getItem itemId
  case mitem of
    Just item -> returnJson item
    Nothing -> invalidArgs [T.pack "unknown item ID"]

postItemReadR :: D.FeedId -> D.ItemId -> Handler ()
postItemReadR = setBoolValueR F.setItemRead

postItemStarredR :: D.FeedId -> D.ItemId -> Handler ()
postItemStarredR = setBoolValueR F.setItemStarred

postFeedReadAllR :: D.FeedId -> Handler ()
postFeedReadAllR feedId = do
  config <- getConfig
  runDB $ F.markAllAsRead feedId

loadConfiguration :: FilePath -> IO C.Configuration
loadConfiguration file = do
  contents <- readFile file
  case C.parseConfiguration contents of
    Just x -> return x
    Nothing -> error "config parse error"

corsResourcePolicy _ = Just
  Cors.simpleCorsResourcePolicy
    { Cors.corsMethods = "PUT" : "DELETE" : Cors.simpleMethods
    , Cors.corsRequestHeaders = Cors.simpleHeaders
    }

main :: IO ()
main = do
  [arg] <- getArgs
  config@C.Configuration{..} <- loadConfiguration arg
  runStderrLoggingT $
    withSqlitePool (T.pack database) numDatabaseConnections $ \pool ->
      liftIO $ do
        startFetcher pool refreshDelayMicros
        app <- toWaiApp (App config pool)
        run 3000 (Cors.cors corsResourcePolicy app)
