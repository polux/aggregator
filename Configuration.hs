module Configuration (
  Configuration(..),
  parseConfiguration,
  userTitle
) where

import Control.Monad (join)
import Safe (readMay)

data Configuration = Configuration 
  { database :: String
  , feeds :: [(String, Maybe String)] -- list of URL, optionally user-defined title pairs
  } 
  deriving (Show, Read)

parseConfiguration :: String -> Maybe Configuration
parseConfiguration = readMay

userTitle :: Configuration
          -> String        -- URL of the feed
          -> Maybe String  -- optionally used-defined title of the feed
userTitle (Configuration { feeds = feeds }) url = join (lookup url feeds)
