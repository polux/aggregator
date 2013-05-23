{-# LANGUAGE OverloadedStrings #-}

module Feeds (
  Feed(..),
  Item(..),
  feedToData,

  getFeed,
  getAllFeeds,
  getItems,
  setItemRead,
  setItemStarred,
  markAllAsRead,

  insertOrUpdateData,
) where

import qualified Configuration as C
import qualified Text.Feed.Types as F
import qualified Data as D
import qualified Data.Text as T
import Text.Feed.Query
import Database.Persist
import Data.Aeson
import Data.Maybe (maybeToList, catMaybes)
import Data.Time.Clock (UTCTime)
import Data.Time.LocalTime (zonedTimeToUTC)
import Data.Time.RFC2822 (readRFC2822)
import Data.Time.RFC3339 (readRFC3339)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds, posixSecondsToUTCTime)
import Control.Applicative ((<|>))
import Web.PathPieces (toPathPiece)
import Html (cleanupHtml)

import qualified Text.RSS.Syntax  as RSS
import qualified Text.Atom.Feed   as Atom
import qualified Text.RSS1.Syntax as RSS1
import qualified Text.XML.Light as XML
import qualified Text.Feed.Types as Feed

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
    , feedUnreadCount :: Int
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
  toJSON (Feed id title unreadCount) =
    object [ "id" .= id
           , "title" .= title
           , "unreadCount" .= unreadCount
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

millisToUtc :: Int -> UTCTime
millisToUtc millis = posixSecondsToUTCTime $ fromRational (toEnum millis / 1000)

utcToMillis :: UTCTime -> Int
utcToMillis utc = round $ utcTimeToPOSIXSeconds utc * 1000

-- parse an RSS or Atom date
parseDate :: String -> Maybe UTCTime
parseDate date = fmap zonedTimeToUTC (readRFC2822 date <|> readRFC3339 date)

-- Extracts the description of a feed item. Replace the feed package's broken
-- getItemDescription function.
extractDescription :: ItemGetter String
extractDescription (Feed.RSSItem  e) = RSS.rssItemDescription e
extractDescription (Feed.RSS1Item i) = RSS1.itemDesc i
extractDescription (Feed.XMLItem _) = Nothing
extractDescription (Feed.AtomItem e) = fmap contentToStr $ Atom.entryContent e
  where contentToStr (Atom.TextContent s) = s
        contentToStr (Atom.HTMLContent s) = s
        contentToStr (Atom.XHTMLContent s) = XML.strContent s

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
                content = (cleanupHtml `fmap` mdescription) `orElse` ""
                  where mdescription = extractDescription item <|> getItemSummary item
                date = (getItemDate item >>= parseDate) `orElse` defaultDate
                author = getItemAuthor item `orElse` ""

-- Inserts or update a data feed and associated Items in the database
insertOrUpdateData :: C.Configuration -> D.Feed -> (D.FeedId -> [D.Item]) -> IO ()
insertOrUpdateData config newFeed newItems = D.runDb config $ do
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
setItemRead :: C.Configuration -> D.ItemId -> Bool -> IO ()
setItemRead config itemKey value = D.runDb config $ update itemKey [ D.ItemRead =. value ] 

-- set an Item's 'read' status
setItemStarred :: C.Configuration -> D.ItemId -> Bool -> IO ()
setItemStarred config itemKey value = D.runDb config $ update itemKey [ D.ItemStarred =. value ] 
-- marks all items in a feed as read
markAllAsRead :: C.Configuration -> D.FeedId -> IO ()
markAllAsRead config feedKey = D.runDb config $ do
  itemKeys <- selectKeysList [D.ItemParent ==. feedKey, D.ItemRead ==. False] []
  mapM_ markAsRead itemKeys
    where markAsRead itemKey = update itemKey [ D.ItemRead =. True ]
  
-- get all items in a feed
getItems :: C.Configuration
         -> D.FeedId
         -> Maybe Int -- end date
         -> Maybe Int -- max items returned
         -> IO [Item]
getItems config feedKey mend mmax = D.runDb config $ do
  items <- selectList filters options
  return $ map dataToMessageItem items

  where filters = [D.ItemParent ==. feedKey]
                ++ [D.ItemDate <. millisToUtc end | end <- maybeToList mend]

        options =  [Desc D.ItemDate]
                ++ [LimitTo max | max <- maybeToList mmax ]
  

-- converts a database item entity into a message
dataToMessageItem ::  Entity D.Item -> Item
dataToMessageItem (Entity k (D.Item _ _ title url content date author starred read)) = 
  Item (toPathPiece k) (utcToMillis date) title content url author starred read

-- converts a database feed entity and a list of message items into a message feed
dataToMessageFeed config feedKey (D.Feed title url) = do
  items <- selectKeysList [D.ItemParent ==. feedKey, D.ItemRead ==. False] []
  return $ Feed (toPathPiece feedKey) bestTitle (length items)
    where bestTitle = (C.userTitle config url) `orElse` title


-- get a feed by id
getFeed :: C.Configuration -> D.FeedId -> IO (Maybe Feed)
getFeed config feedId = D.runDb config $ do
  mfeed <- get feedId
  case mfeed of
    Just feed -> do
      result <- dataToMessageFeed config feedId feed
      return (Just result)
    Nothing -> return Nothing

-- get all the feeds from the database as messages
getAllFeeds :: C.Configuration -> IO [Feed]
getAllFeeds config = D.runDb config $ do
  feeds <- catMaybes `fmap` mapM getUnique (C.feeds config)
  mapM fill feeds
  
  where getUnique (origin, _) = getBy $ D.UniqueOrigin origin
        fill (Entity key feed) = dataToMessageFeed config key feed
