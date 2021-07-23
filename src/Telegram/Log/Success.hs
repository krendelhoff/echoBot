module Telegram.Log.Success
  ( logSuccess
  , logCommandSuccess
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Prelude                hiding (repeat)

import           Telegram.Configuration
import           Telegram.Log
import qualified Telegram.ParseJSON     as PJ

logSuccess ::
     String -> PJ.Message -> StateT (Config, Map Int Int) (ExceptT String IO) ()
logSuccess mode m = do
  (lvl, rep) <- gets getter
  when
    (lvl >= ALL)
    (liftIO $
     writeLog ALL $
     "Successfully echoed " <> mode <> " " <> show rep <> " times")
  where
    getter (config, map) = (logMode config, rep)
      where
        rep = maybe (repeat $ config) id $ M.lookup (PJ.id $ PJ.chat $ m) map

logCommandSuccess ::
     String -> StateT (Config, Map Int Int) (ExceptT String IO) ()
logCommandSuccess mode = do
  lvl <- gets getter
  when
    (lvl >= ALL)
    (liftIO $ writeLog ALL $ "Successfully executed " <> mode <> " command")
  where
    getter (config, _) = logMode config
