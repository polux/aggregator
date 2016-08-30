module Configuration (
  Configuration(..),
  parseConfiguration,
) where

import Control.Monad (join)
import Safe (readMay)

data Configuration = Configuration
  { database :: String
  }
  deriving (Show, Read)

parseConfiguration :: String -> Maybe Configuration
parseConfiguration = readMay
