module Telegram.Echo.SendMessage
  ( sendMessageRequest
  ) where

import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple

-- вспомнить про то как используя ExceptT накапливать все ошибки, а не останавливаться на первой(stepik)
sendMessageRequest :: BC.ByteString -> BC.ByteString -> Request
sendMessageRequest idValue textValue =
  setRequestQueryString [("chat_id", Just idValue), ("text", Just textValue)] $
  setRequestMethod "POST" $
  setRequestSecure True $
  "https://api.telegram.org/bot1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU/sendMessage"
