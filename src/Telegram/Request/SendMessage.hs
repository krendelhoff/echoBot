module Telegram.Request.SendMessage
  ( sendMessageRequest
  ) where

import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple

import           Telegram.Request

sendMessageRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
sendMessageRequest idValue textValue token =
  createRequest
    "POST"
    token
    "sendMessage"
    [("chat_id", Just idValue), ("text", Just textValue)]
