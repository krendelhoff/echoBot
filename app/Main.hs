module Main where

import           Control.Monad.Except
import           Data.IORef

import           GetUpdates
import           ParseJSON

bot :: IORef Int -> ExceptT String IO ()
bot lastOffset = do
  offsetValue <- liftIO $ readIORef lastOffset
  updatesJSON <- getUpdates offsetValue
  updates <- parseUpdatesJSON updatesJSON
  return ()

main :: IO ()
main = do
  lastOffset <- newIORef 0
  forever $ do
    res <- runExceptT $ bot lastOffset
    either putStrLn (const $ return ()) res
