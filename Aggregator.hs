{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import qualified Data as D
import qualified Feeds as F
import qualified Configuration as C
import Fetcher
import Safe (readMay)
import Data.Text as T
import System.Environment (getArgs)

data HelloWorld = HelloWorld { configuration :: C.Configuration }

mkYesod "HelloWorld" [parseRoutes|
/feeds FeedsR GET
/feeds/#D.FeedId FeedR GET
/feeds/#D.FeedId/items ItemsR GET
/feeds/#D.FeedId/readAll FeedReadAllR POST
/items/#D.ItemId/read ItemReadR POST
/items/#D.ItemId/starred ItemStarredR POST
|]

instance Yesod HelloWorld

withAuth :: Handler a -> Handler a
withAuth handler = do
  addHeader "Access-Control-Allow-Origin" "*"
  handler

getConfig = configuration `fmap` getYesod
decode mx = mx >>= readMay . T.unpack

getFeedsR :: Handler Value
getFeedsR = withAuth $ do
  config <- getConfig
  feeds <- liftIO $ F.getAllFeeds config
  jsonToRepJson feeds

getFeedR :: D.FeedId -> Handler Value
getFeedR feedId = withAuth $ do
  config <- getConfig
  mfeed <- liftIO $ F.getFeed config feedId
  case mfeed of
    Just feed -> jsonToRepJson feed
    Nothing -> invalidArgs [T.pack "unknown feed ID"]

getItemsR :: D.FeedId -> Handler Value
getItemsR feedId = withAuth $ do
  mend <- lookupGetParam "end"
  mmax <- lookupGetParam "max"
  config <- getConfig
  items <- liftIO $ F.getItems config feedId (decode mend) (decode mmax)
  jsonToRepJson items

setBoolValueR :: (C.Configuration -> D.ItemId -> Bool -> IO ()) -> D.ItemId -> Handler ()
setBoolValueR setter itemId = withAuth $ do
  mvalue <- lookupPostParam "value"
  case decode mvalue of
    Just value -> do
      config <- getConfig
      liftIO $ setter config itemId value
    Nothing -> invalidArgs [T.pack "value"]


postItemReadR :: D.ItemId -> Handler ()
postItemReadR = setBoolValueR F.setItemRead

postItemStarredR :: D.ItemId -> Handler ()
postItemStarredR = setBoolValueR F.setItemStarred

postFeedReadAllR :: D.FeedId -> Handler ()
postFeedReadAllR feedId = do
  config <- getConfig
  liftIO $ F.markAllAsRead config feedId

loadConfiguration :: FilePath -> IO C.Configuration
loadConfiguration file = do
  contents <- readFile file
  case C.parseConfiguration contents of
    Just x -> return x
    Nothing -> error "config parse error"

main :: IO ()
main = do
  [arg] <- getArgs
  config <- loadConfiguration arg
  D.initializeDb config
  startFetcher config
  warp 3000 (HelloWorld config)
