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
