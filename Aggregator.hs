{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses,
             TemplateHaskell, OverloadedStrings #-}
import Yesod
import Data
import Feeds
import Fetcher

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/feeds FeedsR GET
|]

instance Yesod HelloWorld

getFeedsR :: Handler RepJson
getFeedsR = do
  setHeader "Access-Control-Allow-Origin" "*"
  feeds <- liftIO $ getAllFeeds
  jsonToRepJson feeds

main :: IO ()
main = do
  initializeDb
  startFetcher
  warpDebug 3000 HelloWorld
