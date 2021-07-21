module Telegram.Request.SendMessage
  ( sendMessageRequest
  ) where

import qualified Data.ByteString.Char8 as BC
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.Request

sendMessageRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
sendMessageRequest idValue textValue token =
  createRequest
    "POST"
    token
    "sendMessage"
    [("chat_id", Just idValue), ("text", Just textValue)]

instance AttemptRequest Text where
  tryPerformRequest _ _ Nothing              = return ()
  tryPerformRequest userid token (Just text) = echo userid token text

instance EchoRequest Text where
  echo userid token text =
    performEchoRequest (sendMessageRequest userid (encodeUtf8 text) token)
