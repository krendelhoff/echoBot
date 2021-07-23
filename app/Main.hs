module Main where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Char8       as BC
import           Data.IORef
import           Data.Map                    (Map)
import qualified Data.Map                    as M

import           Telegram.Configuration
import           Telegram.Echo
import           Telegram.ParseJSON
import           Telegram.Request.GetUpdates

refreshOffset :: Updates -> Int -> Int
refreshOffset (Updates {result = []}) offsetValue = offsetValue
refreshOffset updates _ = succ . update_id . last . result $ updates

bot :: IORef Int -> StateT (Config, Map Int Int) (ExceptT String IO) ()
bot lastOffset = do
  offsetValue <- liftIO $ readIORef lastOffset
  updatesJSON <- getUpdates (BC.pack $ show $ offsetValue)
  updates <- lift $ parseUpdatesJSON updatesJSON
  liftIO $ writeIORef lastOffset (refreshOffset updates offsetValue)
  echoBot updates

main :: IO ()
main = do
  lastOffset <- newIORef 0
  eitherConfig <- runExceptT parseConfig
  either
    putStrLn
    (\config -> do
       forever $ do
         res <- runExceptT $ runStateT (bot lastOffset) (config, M.empty)
         either putStrLn (const $ return ()) res)
    eitherConfig
