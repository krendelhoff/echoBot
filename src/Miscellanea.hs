module Miscellanea where

import           Data.Text (Text, pack)
import           Relude

showText :: Show a => a -> Text
showText = pack . show
