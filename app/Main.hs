module Main where

import           Logger                (Mode (..), Priority (..), log)
import qualified Logger
import qualified Logger.Display
import           Miscellanea
import qualified Network.Request       as Request
import qualified Network.Request.Imp   as Request.Imp

import           Control.Exception
import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BC (readFile)
import qualified Data.Text.IO          as TIO
import           Relude
import qualified System.IO             as SIO

type EarlyReader = (Request.Config, Logger.Handle)

main :: IO ()
main = do
  rawData <- readConfig
  cLogger <- Logger.Display.parseConfig rawData
  cRequest <- Request.parseConfig rawData
  Logger.Display.withHandle cLogger $ \hLogger -> do
    runBot bot (cRequest, hLogger)

runBot bot rdr = runReaderT (bot) rdr

bot :: (MonadIO m, MonadReader EarlyReader m) => m ()
bot = do
  return ()

readConfig =
  BC.readFile "config.yaml" `catch`
  (\exception -> do
     TIO.putStrLn "Can't open crucial config.yaml file!"
     TIO.putStrLn $ "Here is what happened:"
     TIO.putStrLn $ showText (exception :: SomeException)
     exitFailure)
