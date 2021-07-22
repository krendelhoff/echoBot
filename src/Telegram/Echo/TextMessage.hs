module Telegram.Echo.TextMessage
  ( echoTextMessage
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text                    (pack)
import           Data.Text.Encoding           (encodeUtf8)

import           Telegram.Configuration
import qualified Telegram.ParseJSON           as PJ
import           Telegram.Request
import           Telegram.Request.SendMessage

echoTextMessage :: PJ.Message -> ReaderT Config (ExceptT String IO) ()
echoTextMessage m = do
  let userId = encodeUtf8 $ pack $ show $ PJ.id $ PJ.chat m
      text = encodeUtf8 $ PJ.text $ m
  tokn <- asks (encodeUtf8 . token)
  lift $ performEchoRequest $ sendMessageRequest userId text tokn
