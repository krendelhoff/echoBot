module Main where

import           Control.Monad.Except
import qualified Data.ByteString.Char8  as BC
import           Data.IORef

import           Telegram.Configuration
import           Telegram.Echo
import           Telegram.GetUpdates
import           Telegram.ParseJSON

refreshOffset :: Updates -> Int -> Int
refreshOffset (Updates {result = []}) offsetValue = succ offsetValue
refreshOffset updates _ = succ . update_id . last . result $ updates

bot :: IORef Int -> ExceptT String IO ()
bot lastOffset = do
  offsetValue <- liftIO $ readIORef lastOffset
  env <- parseConfig
  liftIO $ print env
  updatesJSON <- getUpdates offsetValue
  updates <- parseUpdatesJSON updatesJSON
  liftIO $ writeIORef lastOffset (refreshOffset updates offsetValue)
  echo updates

debug :: ExceptT String IO ()
debug = do
  updatesJSON <- getUpdates 0
  updates <- parseUpdatesJSON updatesJSON
  liftIO $ BC.writeFile "updates.json" updatesJSON
  liftIO $ print updates

main :: IO ()
main = do
  lastOffset <- newIORef 0
  forever $ do
    res <- runExceptT $ bot lastOffset
    either putStrLn (const $ return ()) res
