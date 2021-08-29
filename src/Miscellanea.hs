module Miscellanea where

import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text, pack)
import           Relude

showText :: Show a => a -> Text
showText = pack . show

showBS :: Show a => a -> ByteString
showBS = encodeUtf8 . showText
