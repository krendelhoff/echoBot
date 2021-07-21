module Telegram.Echo
  ( echoBot
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Char8        as BC
import           Data.Text                    (Text)
import           Data.Text.Encoding           (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.Configuration
import qualified Telegram.ParseJSON           as PJ
import           Telegram.Request
import           Telegram.Request.SendMessage
import           Telegram.Request.SendSticker

type UserId = BC.ByteString

type Token = BC.ByteString

echoBot :: PJ.Updates -> ReaderT Config (ExceptT String IO) ()
echoBot updates = do
  tokenValue <- asks (encodeUtf8 . token)
  forM_
    (PJ.result updates)
    (\update -> do
       let idValue = BC.pack $ show (PJ.id $ PJ.chat $ PJ.message $ update)
           textMessage = PJ.text $ PJ.message $ update
           sticker = PJ.sticker $ PJ.message $ update
       lift $ tryPerformRequest idValue tokenValue sticker
       lift $ tryPerformRequest idValue tokenValue textMessage)
