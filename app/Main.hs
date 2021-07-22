module Main where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Char8       as BC
import           Data.IORef

import           Telegram.Configuration
import           Telegram.Echo
import           Telegram.ParseJSON
import           Telegram.Request.GetUpdates

refreshOffset :: Updates -> Int -> Int
refreshOffset (Updates {result = []}) offsetValue = offsetValue
refreshOffset updates _ = succ . update_id . last . result $ updates

bot :: IORef Int -> ReaderT Config (ExceptT String IO) ()
bot lastOffset = do
  offsetValue <- liftIO $ readIORef lastOffset
  updatesJSON <- getUpdates offsetValue
  liftIO $ print updatesJSON
  updates <- lift $ parseUpdatesJSON updatesJSON
  liftIO $ print updates
  liftIO $ writeIORef lastOffset (refreshOffset updates offsetValue)

main :: IO ()
main = do
  lastOffset <- newIORef 0
  eitherConfig <- runExceptT parseConfig
  either
    putStrLn
    (\config -> do
       forever $ do
         res <- runExceptT $ runReaderT (bot lastOffset) config
         either putStrLn (const $ return ()) res)
    eitherConfig
