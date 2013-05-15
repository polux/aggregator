{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings,
             GADTs, FlexibleContexts, EmptyDataDecls #-}

module Data where

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (runResourceT)

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

runDb = runSqlite "foo"

initializeDb :: IO ()
initializeDb = runDb (runMigration migrateAll)

