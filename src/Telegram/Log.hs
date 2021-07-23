module Telegram.Log
  ( LogLevel(..)
  , writeLog
  ) where

import           Data.Yaml
import           GHC.Generics

data LogLevel
  = ERROR
  | WARN
  | ALL
  deriving (Show, Eq, Ord, Generic)

instance FromJSON LogLevel

writeLog :: LogLevel -> String -> IO ()
writeLog lvl text = appendFile "log.txt" $ show lvl ++ ":" ++ text ++ "\n"
