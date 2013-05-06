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
|]

instance Yesod HelloWorld

getFeedsR :: Handler RepJson
getFeedsR = do
  setHeader "Access-Control-Allow-Origin" "*"
  feeds <- liftIO $ F.getAllFeeds
  jsonToRepJson feeds

postItemReadR :: D.ItemId -> Handler ()
postItemReadR itemKey = do
  mvalue <- lookupPostParam "value"
  case mvalue >>= readMay . T.unpack of
    Just value -> liftIO $ F.setItemRead itemKey value
    Nothing -> invalidArgs [T.pack "value"]

main :: IO ()
main = do
  initializeDb
  startFetcher
  warpDebug 3000 HelloWorld
