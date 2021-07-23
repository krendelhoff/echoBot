module Telegram.Echo
  ( echoBot
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map                     (Map)
import           Data.Text.Encoding           (encodeUtf8)

import           Telegram.Configuration
import           Telegram.Echo.Photo
import           Telegram.Echo.Sticker
import           Telegram.Echo.TextMessage
import           Telegram.ParseJSON
import           Telegram.Request
import           Telegram.Request.SendMessage
import           Telegram.Request.SendPhoto
import           Telegram.Request.SendSticker

echo :: Message -> StateT (Config, Map Int Int) (ExceptT String IO) ()
echo m@(TextMessage {})    = processTextMessage m
echo m@(PhotoMessage {})   = echoPhotoMessage m
echo m@(StickerMessage {}) = echoSticker m

echoBot :: Updates -> StateT (Config, Map Int Int) (ExceptT String IO) ()
echoBot updates = do
  forM_ (result updates) (echo . message)
