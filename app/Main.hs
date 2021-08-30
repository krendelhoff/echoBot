module Main where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Request
import           Logger                (Mode (..), Priority (..), log)
import qualified Logger
import qualified Logger.Display
import           Miscellanea
import qualified Network.Request       as Request
import qualified Network.Request.Imp   as Request.Imp
import qualified ParseJSON

import           Control.Exception
import           Control.Monad.Except
import           Data.Aeson            (eitherDecodeStrict')
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

bot :: (MonadIO m, MonadReader Env m) => m ()
bot = do
  Env {..} <- ask
  {-- READING MUTABLE STATE --}
  offset <- readIORef offsetRef
  -----------------------------
  let getUpdates = Data.Request.getUpdates offset funcConfig
      makeRequest req =
        Request.Imp.withHandle
          reqConfig
          hLogger
          req
          (runExceptT . Request.perform)
  result <- liftIO $ makeRequest getUpdates
  when (isRight result) $ do
    case updatesEither result of
      Left err ->
        liftIO $ log hLogger Error "Got incorrect getUpdates JSON data"
      Right ParseJSON.Updates {result = updates, ..} -> do
        forM_ updates $ \ParseJSON.Update {..} -> do
          return ()
          {-- MUTATING STATE --}
          writeIORef offsetRef $ succ update_id
          ---------------------- -- вот с этого момента можно новое монадическое
          --                        действие начать, апдейт как аргумент
          --                        остальное с Reader
          let copyMessage = Data.Request.copyMessage id message_id
          case text of
            Nothing -> do
              liftIO $ makeRequest copyMessage -- TODO check
            Just txt -> do
              case txt of
                "/help" -> do
                  let helpMessage = Data.Request.helpMessage id funcConfig
                  liftIO $ makeRequest helpMessage -- TODO check
                "/repeat" -> do
                  let repeatMessage = Data.Request.repeatMessage id funcConfig
                  liftIO $ makeRequest repeatMessage
                  -- TODO mutate RepeatMap
                _ -> do
                  liftIO $ makeRequest copyMessage -- TODO check

readConfig =
  BC.readFile "config.yaml" `catch`
  (\exception -> do
     TIO.putStrLn "Can't open crucial config.yaml file!"
     TIO.putStrLn $ "Here is what happened:"
     TIO.putStrLn $ showText (exception :: SomeException)
     exitFailure)

runBot bot rdr = runReaderT (bot) rdr

updatesEither result =
  let rawJSON = fromRight "" result
   in eitherDecodeStrict' rawJSON
