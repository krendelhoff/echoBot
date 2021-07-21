module Telegram.Request.SendSticker
  ( sendStickerRequest
  ) where

import qualified Data.ByteString.Char8 as BC
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.ParseJSON
import           Telegram.Request

sendStickerRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
sendStickerRequest chat_id sticker token =
  createRequest
    "POST"
    token
    "sendSticker"
    [("chat_id", Just chat_id), ("sticker", Just sticker)]

instance AttemptRequest Sticker where
  tryPerformRequest _ _ Nothing                 = return ()
  tryPerformRequest userid token (Just sticker) = echo userid token sticker

instance EchoRequest Sticker where
  echo userid token sticker =
    performEchoRequest $
    sendStickerRequest userid (encodeUtf8 . file_id $ sticker) token
