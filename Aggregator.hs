{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Data as D
import Feeds as F
import Fetcher
import Safe (readMay)
import Data.Text as T

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/feeds FeedsR GET
/items/#D.ItemId/read ItemReadR POST
/items/#D.ItemId/starred ItemStarredR POST
|]

instance Yesod HelloWorld

withAuth :: Handler a -> Handler a
withAuth handler = do
  addHeader "Access-Control-Allow-Origin" "*"
  handler

getFeedsR :: Handler Value
getFeedsR = withAuth $ do
  feeds <- liftIO $ F.getAllFeeds
  jsonToRepJson feeds

setBoolValueR :: (D.ItemId -> Bool -> IO ()) -> D.ItemId -> Handler ()
setBoolValueR setter itemId = withAuth $ do
  mvalue <- lookupPostParam "value"
  case mvalue >>= readMay . T.unpack of
    Just value -> liftIO $ setter itemId value
    Nothing -> invalidArgs [T.pack "value"]


postItemReadR :: D.ItemId -> Handler ()
postItemReadR = setBoolValueR setItemRead

postItemStarredR :: D.ItemId -> Handler ()
postItemStarredR = setBoolValueR setItemStarred

main :: IO ()
main = do
  initializeDb
  startFetcher
  warp 3000 HelloWorld
