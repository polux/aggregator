module Configuration (
  Configuration(..),
  parseConfiguration
) where

import Safe (readMay)

data Configuration = Configuration 
  { database :: String
  , feeds :: [String]
  } 
  deriving (Show, Read)

parseConfiguration :: String -> Maybe Configuration
parseConfiguration = readMay
