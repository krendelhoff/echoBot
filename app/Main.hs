module Main where

import           Data.Request          (addQuery)
import qualified Data.Request
import           Logger                (Mode (..), Priority (..), log)
import qualified Logger
import qualified Logger.Display
import           Miscellanea
import qualified Network.Request       as Request
import qualified Network.Request.Imp   as Request.Imp
import           ParseJSON             (CallbackQuery (..), Message (..),
                                        UpdateType (CallbackQueryType, MessageType))
import qualified ParseJSON

import           Control.Exception
import           Control.Monad.Catch   hiding (catch)
import           Control.Monad.Except
import           Data.Aeson            (eitherDecodeStrict')
import qualified Data.ByteString.Char8 as BC
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
  -- если конфига нет, создать с дефолтными и попросить ввести токен
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
  result <- makeRequest env getUpdates
  when (isRight result) $ do
    case updatesEither result of
      Left err ->
        liftIO $ log hLogger Error "Got incorrect getUpdates JSON data"
      Right ParseJSON.Updates {result = updates, ..} -> do
        forM_ updates processUpdate

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
  -- any text?
  case text of
    Nothing -> do
      makeMultipleRequest env repeat update copyMessage
    Just txt -> do
      return ()
      -- command or message?
      case txt of
        "/help" -> do
          let helpMessage =
                Data.Request.sendMessage id (Data.Request.help funcConfig)
          result <- makeRequest env helpMessage
          let toCheck = fromRight "BAD RESULT" result
          checkSuccess "sendMessage" hLogger toCheck update
        "/repeat" -> do
          let repeatMessage =
                Data.Request.sendMessage
                  id
                  (Data.Request.question funcConfig <> ": " <> showText repeat) `addQuery`
                Data.Request.addKeyboardMarkup
          result <- makeRequest env repeatMessage
          let toCheck = fromRight "BAD RESULT" result
          checkSuccess "sendMessage" hLogger toCheck update
        _ -> do
          makeMultipleRequest env repeat update copyMessage
processUpdate update@ParseJSON.Update { update = CallbackQueryType (CallbackQuery {..})
                                      , ..
                                      } = do
  env@Env {..} <- ask
  {--------- MUTATING STATE ----------}
  writeIORef offsetRef $ succ update_id
  -------------------------------------
  let newRepeatMaybe = readMaybe (T.unpack callback_data)
  case newRepeatMaybe of
    Nothing -> return ()
    Just newRepeat -> do
      modifyIORef repeatMapRef (M.insert user_id newRepeat)
      let answerCallbackQuery = Data.Request.answerCallbackQuery callback_id
      result <- makeRequest env answerCallbackQuery
      let toCheck = fromRight "BAD RESULT" result
      checkSuccess "answerCallbackQuery" hLogger toCheck update

readConfig =
  BC.readFile "config.yaml" `catch`
  (\exception -> do
     TIO.putStrLn "Can't open crucial config.yaml file!"
     TIO.putStrLn $ "Here is what happened:"
     TIO.putStrLn $ showText (exception :: SomeException)
     exitFailure)

runBot = runReaderT

updatesEither result =
  let rawJSON = fromRight "" result
   in eitherDecodeStrict' rawJSON

makeRequest ::
     (MonadReader Env m, MonadIO m)
  => Env
  -> Data.Request.Info
  -> m (Either Request.Error ByteString)
makeRequest Env {..} req =
  liftIO $
  Request.Imp.withHandle reqConfig hLogger req (runExceptT . Request.perform)

makeMultipleRequest ::
     (MonadReader Env m, MonadIO m)
  => Env
  -> Int
  -> ParseJSON.Update
  -> Data.Request.Info
  -> m ()
makeMultipleRequest env repeat update req = do
  Env {..} <- ask
  eitherResultList <- replicateM repeat $ makeRequest env req
  let resultList = map (fromRight "BAD RESULT") eitherResultList
  forM_ resultList $ \result -> checkSuccess "copyMessage" hLogger result update

howMuch Env {..} ParseJSON.Update {update = MessageType (Message {..}), ..} repeatMap =
  maybe (Data.Request.repeat funcConfig) (\a -> a) (M.lookup id repeatMap)
howMuch _ _ _ = 1

createTextError method =
  "Result decoding failed, can't check " <> method <> " request success"

apiError = "Got incorrect response, API is broken"

approved m = m <> " request acknowledged!"

notApproved m = m <> " request is not acknowledged!"

checkSuccess ::
     (MonadReader Env m, MonadIO m)
  => Text
  -> Logger.Handle
  -> ByteString
  -> ParseJSON.Update
  -> m ()
checkSuccess _ _ "BAD RESULT" _ = return ()
checkSuccess m@"sendMessage" hLogger result update@ParseJSON.Update {update = MessageType message} = do
  let eitherAnswer = eitherDecodeStrict' result
  case eitherAnswer of
    Left err -> do
      liftIO $ log hLogger Error $ createTextError m
    Right (ParseJSON.Msg ok msg) ->
      liftIO $
      if ok -- parsing to message is enough to ack
        then log hLogger Info $ approved m
        else log hLogger Warn $ notApproved m
    Right _ -> liftIO $ log hLogger Warn $ apiError
checkSuccess "sendMessage" _ _ _ = return ()
checkSuccess m@"copyMessage" hLogger result update@ParseJSON.Update {update = MessageType (Message {..})} = do
  let eitherAnswer = eitherDecodeStrict' result
  case eitherAnswer of
    Left err -> do
      liftIO $ log hLogger Error $ createTextError m
    Right (ParseJSON.CopyMsg ok msg_id) ->
      liftIO $
      if ok == True -- parsing to message_id is enough
        then log hLogger Info $ approved m
        else log hLogger Warn $ notApproved m
    Right _ -> liftIO $ log hLogger Warn $ apiError
checkSuccess "copyMessage" _ _ _ = return ()
checkSuccess m@"answerCallbackQuery" hLogger result _ = do
  let eitherAnswer = eitherDecodeStrict' result
  case eitherAnswer of
    Left err -> do
      liftIO $ log hLogger Error $ createTextError m
    Right (ParseJSON.Cbq ok ok_) ->
      liftIO $
      if ok && ok_
        then log hLogger Info $ approved m
        else log hLogger Warn $ notApproved m
    Right _ -> liftIO $ log hLogger Warn $ apiError
checkSuccess _ _ _ _ = return ()
