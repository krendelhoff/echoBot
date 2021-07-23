module Telegram.Echo.TextMessage
  ( echoTextMessage
  , processTextMessage
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Char8        as BC
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Text                    (pack)
import           Data.Text.Encoding           (encodeUtf8)
import           Prelude                      hiding (repeat)

import           Telegram.Configuration
import           Telegram.Echo.Repeat
import           Telegram.Log
import           Telegram.Log.Success
import qualified Telegram.ParseJSON           as PJ
import           Telegram.Request
import           Telegram.Request.SendMessage

processTextMessage ::
     PJ.Message -> StateT (Config, Map Int Int) (ExceptT String IO) ()
processTextMessage m = do
  let userId = encodeUtf8 $ pack $ show $ PJ.id $ PJ.chat m
      text = encodeUtf8 $ PJ.text $ m
  case PJ.text m of
    "/help" -> do
      helpText <- gets getter
      processCommand userId helpText m
      logCommandSuccess "/help"
      where getter (config, _) = encodeUtf8 $ help $ config
    "/repeat" -> do
      performRepeatChange m
      logCommandSuccess "/repeat"
    _ -> do
      echoTextMessage userId text m
      logSuccess "text" m

echoTextMessage ::
     BC.ByteString
  -> BC.ByteString
  -> PJ.Message
  -> StateT (Config, Map Int Int) (ExceptT String IO) ()
echoTextMessage userId text m = do
  tokn <- gets getter
  performEchoRequest (sendMessageRequest userId text tokn) (PJ.id $ PJ.chat $ m)
  where
    getter (config, _) = encodeUtf8 $ token $ config

processCommand ::
     BC.ByteString
  -> BC.ByteString
  -> PJ.Message
  -> StateT (Config, Map Int Int) (ExceptT String IO) ()
processCommand userId text m = do
  tokn <- gets getter
  performCommandRequest (sendMessageRequest userId text tokn)
  where
    getter (config, _) = encodeUtf8 $ token $ config
