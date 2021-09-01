module Main where

import qualified Data.ByteString.Char8 as BC
import           Data.Request          (addQuery)
import qualified Data.Request
import           Logger                (Mode (..), Priority (..), log)
import qualified Logger
import qualified Logger.Display
import           Miscellanea
import qualified Network.Request       as Request
import qualified Network.Request.Imp   as Request.Imp
import           ParseJSON             (Message (..), UpdateType (MessageType))
import qualified ParseJSON

import           Control.Exception
import           Control.Monad.Except
import           Data.Aeson            (eitherDecodeStrict')
import qualified Data.ByteString.Char8 as BC (readFile)
import           Data.Either           (fromRight, isRight)
import qualified Data.Map              as M
import           Data.Text             (Text)
import qualified Data.Text             as T
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
  env@Env {..} <- ask
  {-- READING MUTABLE STATE --}
  offset <- readIORef offsetRef
  -----------------------------
  let getUpdates = Data.Request.getUpdates offset funcConfig
  result <- liftIO $ makeRequest env getUpdates
  when (isRight result) $ do
    liftIO $ BC.putStrLn (fromRight "" result)
    liftIO $
      print
        ((eitherDecodeStrict' (fromRight "" result)) :: Either String ParseJSON.Updates) {-
    case updatesEither result of
      Left err ->
        liftIO $ log hLogger Error "Got incorrect getUpdates JSON data"
      Right ParseJSON.Updates {result = updates, ..} -> do
        forM_ updates processUpdate
        -}

processUpdate :: (MonadReader Env m, MonadIO m) => ParseJSON.Update -> m ()
processUpdate update@ParseJSON.Update {update = MessageType (Message {..}), ..} = do
  env@Env {..} <- ask
  {-- MUTATING STATE --}
  writeIORef offsetRef $ succ update_id
  ----------------------
  let copyMessage = Data.Request.copyMessage id message_id
  {----- READING MUTABLE STATE -----}
  repeatMap <- readIORef repeatMapRef
  -----------------------------------
  let repeat = howMuch env update repeatMap
  case text of
    Nothing -> do
      makeMultipleRequest env repeat copyMessage -- TODO check
    Just txt -> do
      case txt of
        "/help" -> do
          let helpMessage =
                Data.Request.sendMessage id (Data.Request.help funcConfig)
          liftIO $ makeRequest env helpMessage -- TODO check
          return ()
        "/repeat" -> do
          let repeatMessage =
                Data.Request.sendMessage
                  id
                  (Data.Request.question funcConfig <> ": " <> showText repeat) `addQuery`
                Data.Request.addKeyboardMarkup
          liftIO $ makeRequest env repeatMessage
          return ()
        _ -> do
          makeMultipleRequest env repeat copyMessage -- TODO check
processUpdate _ = return ()

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

makeRequest Env {..} req =
  Request.Imp.withHandle reqConfig hLogger req (runExceptT . Request.perform)

makeMultipleRequest env repeat = replicateM_ repeat . liftIO . makeRequest env

howMuch Env {..} ParseJSON.Update {update = MessageType (Message {..}), ..} repeatMap =
  maybe (Data.Request.repeat funcConfig) (\a -> a) (M.lookup id repeatMap)
howMuch _ _ _ = 1
