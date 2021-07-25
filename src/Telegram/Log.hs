module Telegram.Log
  ( LogLevel(..)
  , writeLog
  ) where

import           Control.Monad.Trans
import           Data.Yaml
import           GHC.Generics

data LogLevel
  = ERROR
  | WARN
  | ALL
  deriving (Show, Eq, Ord, Generic)

instance FromJSON LogLevel

writeLog :: MonadIO m => LogLevel -> String -> m ()
writeLog lvl text =
  liftIO $ appendFile "log.txt" $ show lvl ++ ":" ++ text ++ "\n"
