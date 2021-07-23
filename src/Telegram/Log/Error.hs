module Telegram.Log.Error
  ( logError
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map               (Map)

import           Telegram.Configuration
import           Telegram.Log

logError :: String -> StateT (Config, Map Int Int) (ExceptT String IO) a
logError err = do
  liftIO $ writeLog ERROR $ err
  throwError err
