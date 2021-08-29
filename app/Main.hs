module Main where

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
import qualified Data.Map              as M
import qualified Data.Text.IO          as TIO
import           Relude
import qualified System.IO             as SIO

-- Handle для сервисов и для подмен реализаций
-- а тут мы стейт приложения закидываем в ReaderT, в принципе все проблемы разрешены, дальше теперь только JSON нормально спарсить
-- и писать
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
  Logger.Display.withHandle cLogger $ \hLogger -> do
    forever $
      -- результат и так прологгируется, но ExceptT позволит прекратить исполнение
      runBot
        bot
        (Env
           { reqConfig = cRequest
           , funcConfig = cFunc
           , hLogger = hLogger
           , offsetRef = offset
           , repeatMapRef = repeatMap
           })
    return ()

runBot bot rdr = runExceptT $ runReaderT (bot) rdr

-- пусть я понапишу кучу case, зато я контролирую ситуацию!
bot :: (MonadIO m, MonadReader Env m) => m ()
bot = do
  Env {..} <- ask
  offset <- readIORef offsetRef
  let getUpdates = Data.Request.getUpdates offset funcConfig
  liftIO $
    Request.Imp.withHandle reqConfig hLogger getUpdates $ \hRequest -> do
      result <- runExceptT $ Request.perform hRequest
      case result of
        Left _        -> return ()
        Right updates -> print updates

readConfig =
  BC.readFile "config.yaml" `catch`
  (\exception -> do
     TIO.putStrLn "Can't open crucial config.yaml file!"
     TIO.putStrLn $ "Here is what happened:"
     TIO.putStrLn $ showText (exception :: SomeException)
     exitFailure)
