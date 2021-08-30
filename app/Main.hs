module Main where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Request
import           Logger                (Mode (..), Priority (..), log)
import qualified Logger
import qualified Logger.Display
import           Miscellanea
import qualified Network.Request       as Request
import qualified Network.Request.Imp   as Request.Imp

import           Control.Exception
import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BC (readFile)
import           Data.Either           (fromRight, isRight)
import qualified Data.Map              as M
import qualified Data.Text.IO          as TIO
import           Relude
import qualified System.IO             as SIO

data Env =
  Env
    { reqConfig    :: Request.Config
    , funcConfig   :: Data.Request.Config
    , hLogger      :: Logger.Handle
    , offsetRef    :: IORef Int
    , repeatMapRef :: IORef (M.Map Int Int)
    }

main :: IO ()
main = do
  return ()
  -- reading config
  rawData <- readConfig
  cLogger <- Logger.Display.parseConfig rawData
  cRequest <- Request.parseConfig rawData
  cFunc <- Data.Request.parseConfig rawData
  -- defining all mutable state
  offset <- newIORef 0
  repeatMap <- newIORef M.empty
  -- app begins
  let withLogger = Logger.Display.withHandle cLogger
  withLogger $ \hLogger -> do
    forever $
      runBot
        bot
        (Env
           { reqConfig = cRequest
           , funcConfig = cFunc
           , hLogger = hLogger
           , offsetRef = offset
           , repeatMapRef = repeatMap
           })

runBot bot rdr = runExceptT $ runReaderT (bot) rdr

-- пусть я понапишу кучу case, зато я контролирую ситуацию!
bot :: (MonadIO m, MonadReader Env m) => m ()
bot = do
  Env {..} <- ask
  offset <- readIORef offsetRef
  let getUpdates = Data.Request.getUpdates offset funcConfig
      makeRequest req =
        Request.Imp.withHandle
          reqConfig
          hLogger
          req
          (runExceptT . Request.perform)
  result <- liftIO $ makeRequest getUpdates
  when (isRight result) $ do
    let updates = fromRight "" result
    liftIO $ BC.putStrLn updates
        -- copyMessage

readConfig =
  BC.readFile "config.yaml" `catch`
  (\exception -> do
     TIO.putStrLn "Can't open crucial config.yaml file!"
     TIO.putStrLn $ "Here is what happened:"
     TIO.putStrLn $ showText (exception :: SomeException)
     exitFailure)
