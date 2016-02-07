{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings,
             GADTs, FlexibleContexts, EmptyDataDecls, MultiParamTypeClasses,
             GeneralizedNewtypeDeriving #-}

module Data where

import Configuration
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.Text as T

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistUpperCase|
Item
    parent FeedId
    guid String
    UniqueItem parent guid
    title String
    link String
    content String
    date UTCTime
    author String
    starred Bool
    read Bool
    deriving Show
Feed
    title String
    origin String
    UniqueOrigin origin
|]

runDb config = runSqlite (T.pack $ database config)

initializeDb :: Configuration -> IO ()
initializeDb config = runDb config (runMigration migrateAll)

