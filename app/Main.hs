module Main where

import           Control.Monad.Except
import           Data.IORef

import           Echo
import           GetUpdates
import           ParseJSON

refreshOffset :: Updates -> Int -> Int
refreshOffset (Updates {result = []}) offsetValue = succ offsetValue
refreshOffset updates _ = succ . update_id . last . result $ updates

bot :: IORef Int -> ExceptT String IO ()
bot lastOffset = do
  offsetValue <- liftIO $ readIORef lastOffset
  updatesJSON <- getUpdates offsetValue
  updates <- parseUpdatesJSON updatesJSON
  liftIO $ writeIORef lastOffset (refreshOffset updates offsetValue)
  echo updates

main :: IO ()
main = do
  lastOffset <- newIORef 0
  forever $ do
    res <- runExceptT $ bot lastOffset
    either putStrLn (const $ return ()) res
