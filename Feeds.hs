{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Feeds (
  Feed(..),
  Item(..),
  feedToData,

  getFeed,
  createFeed,
  deleteFeed,
  updateFeed,
  getAllFeeds,
  getAllFeedUrls,

  getItems,
  getItem,
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
import Data.Time.RFC2822 (parseTimeRFC2822)
import Data.Time.RFC3339 (parseTimeRFC3339)
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
    , feedOrigin :: String
    , feedUnreadCount :: Int
    }
  deriving (Show)


data Item = Item
    { itemId :: T.Text
    , parentId :: T.Text
    , itemDate :: Int
    , itemTitle :: String
    , itemStarred :: Bool
    , itemRead :: Bool
    , itemOptionalParts :: Maybe OptionalItemParts
    }
  deriving (Show)

data OptionalItemParts = OptionalItemParts
    { itemUrl :: String
    , itemAuthor :: String
    , itemContent :: String
    }
  deriving (Show)

instance ToJSON Feed where
  toJSON (Feed id title origin unreadCount) =
    object [ "id" .= id
           , "title" .= title
           , "origin" .= origin
           , "unreadCount" .= unreadCount
           ]

instance ToJSON Item where
  toJSON (Item id parentId date title starred read optionalParts) =
    object $ [ "id" .= id
             , "feedId" .= parentId
             , "date" .= date
             , "title" .= title
             , "starred" .= starred
             , "read" .= read
             ]
             ++
             optionalFields optionalParts

    where optionalFields Nothing = []
          optionalFields (Just (OptionalItemParts url author content)) =
            [ "content" .= content
            , "url" .= url
            , "author" .= author
            ]

orElse :: Maybe a -> a -> a
orElse m x = maybe x id m

millisToUtc :: Int -> UTCTime
millisToUtc millis = posixSecondsToUTCTime $ fromRational (toEnum millis / 1000)

utcToMillis :: UTCTime -> Int
utcToMillis utc = round $ utcTimeToPOSIXSeconds utc * 1000

-- parse an RSS or Atom date
parseDate :: String -> Maybe UTCTime
parseDate date = fmap zonedTimeToUTC (parseTimeRFC2822 date <|> parseTimeRFC3339 date)

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
  -> D.FeedId                       -- ID of the feed to convert
  -> F.Feed                         -- feed to convert
  -> [D.Item]
feedToData origin defaultDate feedId feed = map (ditem feedId) (getFeedItems feed)
  where ditem feedId item = D.Item feedId guid title url content date author False False
          where guid = (snd `fmap` getItemId item) `orElse` url
                title = getItemTitle item `orElse` ""
                url = getItemLink item `orElse` ""
                content = (cleanupHtml `fmap` mdescription) `orElse` ""
                  where mdescription = extractDescription item <|> getItemSummary item
                date = (getItemDate item >>= parseDate) `orElse` defaultDate
                author = getItemAuthor item `orElse` ""

-- Inserts or update a data feed and associated Items in the database
insertOrUpdateData :: C.Configuration -> [D.Item] -> IO ()
insertOrUpdateData config newItems = D.runDb config $ do
  mapM_ insertOrUpdateItem newItems

  where insertOrUpdateItem newItem = do
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

-- get one item
getItem :: C.Configuration
        -> D.ItemId
        -> IO (Maybe Item)
getItem config itemId = D.runDb config $ do
  mitem <- get itemId
  return $ fmap (dataToMessageItem False itemId) mitem

-- get all items in a feed
getItems :: C.Configuration
         -> Bool -- descriptions only
         -> Bool -- unread only
         -> Bool -- starred only
         -> Either D.FeedId String -- feed ID or query
         -> Maybe Int -- end date
         -> Maybe Int -- max items returned
         -> IO [Item]
getItems config light unread starred feedIdOrQuery mend mmax = D.runDb config $ do
  items <- selectList filters options
  return $ map entityToMessage items

  where filters = either (\feedKey -> [D.ItemParent ==. feedKey])
                         (\query -> contains D.ItemTitle (words query))
                         feedIdOrQuery
                ++ [D.ItemDate <. millisToUtc end | end <- maybeToList mend]
                ++ (if unread then [D.ItemRead ==. False] else [])
                ++ (if starred then [D.ItemStarred ==. True] else [])

        options =  [Desc D.ItemDate]
                ++ [LimitTo max | max <- maybeToList mmax ]

        entityToMessage (Entity key item) = dataToMessageItem light key item

        contains field values = [like field value | value <- values]

        like field value = Filter field
                                  (Left $ "%" ++ value ++ "%")
                                  (BackendSpecificFilter "like")


-- converts a database item entity into a message
dataToMessageItem :: Bool -- exclude optional parts
                  -> D.ItemId
                  -> D.Item
                  -> Item
dataToMessageItem light itemKey (D.Item parentId _ title url content date author starred read) =
  Item (toPathPiece itemKey)
       (toPathPiece parentId)
       (utcToMillis date)
       title
       starred
       read
       (if light then Nothing else Just (OptionalItemParts url author content))

-- converts a database feed entity and a list of message items into a message feed
dataToMessageFeed feedKey (D.Feed title url) = do
  items <- selectKeysList [D.ItemParent ==. feedKey, D.ItemRead ==. False] []
  return $ Feed (toPathPiece feedKey) title url (length items)

-- get a feed by id
getFeed :: C.Configuration -> D.FeedId -> IO (Maybe Feed)
getFeed config feedId = D.runDb config $ do
  mfeed <- get feedId
  case mfeed of
    Just feed -> do
      result <- dataToMessageFeed feedId feed
      return (Just result)
    Nothing -> return Nothing

-- create a new feed
createFeed
  :: C.Configuration
  -> D.Feed
  -> IO Feed
createFeed config feed = D.runDb config $ do
  feedId <- insert feed
  dataToMessageFeed feedId feed

-- delete a feed given its ID
deleteFeed
  :: C.Configuration
  -> D.FeedId
  -> IO ()
deleteFeed config feedId = D.runDb config $ do
  delete feedId
  deleteWhere [D.ItemParent ==. feedId]

-- replaces a feed's definition
updateFeed
  :: C.Configuration
  -> D.FeedId
  -> D.Feed
  -> IO Feed
updateFeed config feedId feed = D.runDb config $ do
  replace feedId feed
  dataToMessageFeed feedId feed

-- get all the feeds from the database as messages
getAllFeeds :: C.Configuration -> IO [Feed]
getAllFeeds config = D.runDb config $ do
  feeds <- selectList [] []
  mapM fill feeds

  where fill (Entity key feed) = dataToMessageFeed key feed

-- get all feed URLs along with their feed IDs
getAllFeedUrls :: C.Configuration -> IO [(D.FeedId, String)]
getAllFeedUrls config = D.runDb config $ do
  feeds <- selectList [] []
  mapM selectUrl feeds

  where selectUrl (Entity key (D.Feed _ url)) = return (key, url)
