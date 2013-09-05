
module LiName.Utils where

import Data.Text (pack, unpack, Text())



notDots :: String -> Bool
notDots "."  = False
notDots ".." = False
notDots _    = True


byText :: (Text -> Text) -> String -> String
byText f = unpack . f . pack

