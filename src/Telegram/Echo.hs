module Telegram.Echo
  ( echoBot
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
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

echo :: Message -> ReaderT Config (ExceptT String IO) ()
echo m@(TextMessage {})    = echoTextMessage m
echo m@(PhotoMessage {})   = echoPhotoMessage m
echo m@(StickerMessage {}) = echoSticker m

echoBot :: Updates -> ReaderT Config (ExceptT String IO) ()
echoBot updates = do
  forM_ (result updates) (echo . message)
