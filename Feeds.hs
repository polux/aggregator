{-# LANGUAGE OverloadedStrings #-}

module Feeds (
  Feed(..),
  Item(..),
  feedToData,

  getAllFeeds,
  setItemRead,
  setItemStarred,

  insertOrUpdateData,
) where

import qualified Text.Feed.Types as F
import qualified Data as D
import qualified Data.Text as T
import Text.Feed.Query
import Database.Persist
import Data.Aeson
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Time.RFC2822 (readRFC2822)
import Data.Time.RFC3339 (readRFC3339)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Control.Applicative ((<|>))
import Web.PathPieces (toPathPiece)

{-
 - There are three different Feed (resp. Items) datatypes:
 -
 - D.Feed : The one stored in the database, called 'data' below
 - F.Feed : The one from the hackage 'feeds' package, called 'feed' below
 - Feed   : The one for communicating with the clients, called 'message' below
 -
 -}

data Feed = Feed
    { feedId :: T.Text
    , feedTitle :: String
    , feedItems :: [Item]
    }
  deriving (Show)


data Item = Item
    { itemId :: T.Text
    , itemDate :: Int
    , itemTitle :: String
    , itemContent :: String
    , itemUrl :: String
    , itemAuthor :: String
    , itemStarred :: Bool
    , itemRead :: Bool
    } 
  deriving (Show)

instance ToJSON Feed where
  toJSON (Feed id title items) =
    object [ "id" .= id
           , "title" .= title
           , "items" .= items
           ]

instance ToJSON Item where
  toJSON (Item id date title content url author starred read) =
    object [ "id" .= id
           , "date" .= date
           , "title" .= title
           , "content" .= content
           , "url" .= url
           , "author" .= author
           , "starred" .= starred
           , "read" .= read
           ]

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing  x = x 

-- parse an RSS or Atom date
parseDate :: String -> Maybe UTCTime
parseDate date = fmap zonedTimeToUTC (readRFC2822 date <|> readRFC3339 date)

-- converts a 'feeds' package feed into data ready to be inserted
feedToData 
  :: String                         -- origin (unique)
  -> UTCTime                        -- default date
  -> F.Feed                         -- feed to convert
  -> (D.Feed, D.FeedId -> [D.Item])
feedToData origin defaultDate feed = (dfeed, ditems)
  where dfeed = D.Feed (getFeedTitle feed) origin
        ditems feedId = map (ditem feedId) (getFeedItems feed)
        ditem feedId item = D.Item feedId guid title url content date author False False
          where guid = (snd `fmap` getItemId item) `orElse` url
                title = getItemTitle item `orElse` ""
                url = getItemLink item `orElse` ""
                content = getItemDescription item `orElse` ""
                date = (getItemDate item >>= parseDate) `orElse` defaultDate
                author = getItemAuthor item `orElse` ""

-- Inserts or update a data feed and associated Items in the database
insertOrUpdateData :: D.Feed -> (D.FeedId -> [D.Item]) -> IO ()
insertOrUpdateData newFeed newItems = D.runDb $ do
  feedId <- insertOrUpdateFeed 
  mapM_ insertOrUpdateItem (newItems feedId)

  where insertOrUpdateFeed = do
          maybeFeed <- getBy unique
          case maybeFeed of
            Just feed -> return (entityKey feed)
            Nothing -> insert newFeed

          where unique = D.UniqueOrigin (D.feedOrigin newFeed)

        insertOrUpdateItem newItem = do
          maybeItem <- getBy unique
          case maybeItem of
            -- we leave parent, guid, starred and read unchanged on purpose
            Just item -> update (entityKey item) [ D.ItemTitle =. D.itemTitle newItem
                                                 , D.ItemLink =. D.itemLink newItem
                                                 , D.ItemContent =. D.itemContent newItem
                                                 , D.ItemAuthor =. D.itemAuthor newItem
                                                 ]
            Nothing -> do insert newItem
                          return ()

          where unique = D.UniqueItem (D.itemParent newItem) 
                                      (D.itemGuid newItem)

-- set an Item's 'read' status
setItemRead :: D.ItemId -> Bool -> IO ()
setItemRead itemKey value = D.runDb $ update itemKey [ D.ItemRead =. value ] 

-- set an Item's 'read' status
setItemStarred :: D.ItemId -> Bool -> IO ()
setItemStarred itemKey value = D.runDb $ update itemKey [ D.ItemStarred =. value ] 


-- converts a database item entity into a message
dataToItem ::  Entity D.Item -> Item
dataToItem (Entity k (D.Item _ _ title url content date author starred read)) = 
  Item (toPathPiece k) (round $ utcTimeToPOSIXSeconds date) title content url author 
       starred read

-- converts a database feed entity and a list of message items into a message feed
dataToFeed (Entity k (D.Feed title _)) items = Feed (toPathPiece k) title items

-- get all the feeds from the database as messages
getAllFeeds :: IO [Feed]
getAllFeeds = D.runDb $ do
  feeds <- selectList [] []
  mapM fill feeds
  where fill entity = do
          items <- selectList [D.ItemParent ==. entityKey entity] [Desc D.ItemDate]
          return $ dataToFeed entity (map dataToItem items)
