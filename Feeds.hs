{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Feeds (
  Feed(..),
  Item(..),
  DbLike,
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
import Control.Monad.Reader (ReaderT)
import Control.Monad.IO.Class (MonadIO)
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

type DbLike m backend = (MonadIO m, PersistQuery backend, PersistStore backend, PersistUnique backend, PersistRecordBackend D.Feed backend, PersistRecordBackend D.Item backend)

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
insertOrUpdateData :: DbLike m backend => [D.Item] -> ReaderT backend m ()
insertOrUpdateData newItems = do
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
setItemRead :: DbLike m backend => D.ItemId -> Bool -> ReaderT backend m ()
setItemRead itemKey value = update itemKey [ D.ItemRead =. value ]

-- set an Item's 'read' status
setItemStarred :: DbLike m backend => D.ItemId -> Bool -> ReaderT backend m ()
setItemStarred itemKey value = update itemKey [ D.ItemStarred =. value ]

-- marks all items in a feed as read
markAllAsRead :: DbLike m backend => D.FeedId -> ReaderT backend m ()
markAllAsRead feedKey = do
  itemKeys <- selectKeysList [D.ItemParent ==. feedKey, D.ItemRead ==. False] []
  mapM_ markAsRead itemKeys
    where markAsRead itemKey = update itemKey [ D.ItemRead =. True ]

-- get one item
getItem :: DbLike m backend
        => D.ItemId
        -> ReaderT backend m (Maybe Item)
getItem itemId = do
  mitem <- get itemId
  return $ fmap (dataToMessageItem False itemId) mitem

-- get all items in a feed
getItems :: DbLike m backend
         => Bool -- descriptions only
         -> Bool -- unread only
         -> Bool -- starred only
         -> Either D.FeedId String -- feed ID or query
         -> Maybe Int -- end date
         -> Maybe Int -- max items returned
         -> ReaderT backend m [Item]
getItems light unread starred feedIdOrQuery mend mmax = do
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
  numItems <- count [D.ItemParent ==. feedKey, D.ItemRead ==. False]
  return $ Feed (toPathPiece feedKey) title url numItems

-- get a feed by id
getFeed :: DbLike m backend => D.FeedId -> ReaderT backend m (Maybe Feed)
getFeed feedId = do
  mfeed <- get feedId
  case mfeed of
    Just feed -> do
      result <- dataToMessageFeed feedId feed
      return (Just result)
    Nothing -> return Nothing

-- create a new feed
createFeed
  :: DbLike m backend
  => D.Feed
  -> ReaderT backend m Feed
createFeed feed = do
  feedId <- insert feed
  dataToMessageFeed feedId feed

-- delete a feed given its ID
deleteFeed
  :: DbLike m backend
  => D.FeedId
  -> ReaderT backend m ()
deleteFeed feedId = do
  delete feedId
  deleteWhere [D.ItemParent ==. feedId]

-- replaces a feed's definition
updateFeed
  :: DbLike m backend
  => D.FeedId
  -> D.Feed
  -> ReaderT backend m Feed
updateFeed feedId feed = do
  replace feedId feed
  dataToMessageFeed feedId feed


-- get all the feeds from the database as messages
getAllFeeds :: DbLike m backend => ReaderT backend m [Feed]
getAllFeeds = do
  feeds <- selectList [] []
  mapM fill feeds

  where fill (Entity key feed) = dataToMessageFeed key feed

-- get all feed URLs along with their feed IDs
getAllFeedUrls :: DbLike m backend => ReaderT backend m [(D.FeedId, String)]
getAllFeedUrls = do
  feeds <- selectList [] []
  mapM selectUrl feeds

  where selectUrl (Entity key (D.Feed _ url)) = return (key, url)
