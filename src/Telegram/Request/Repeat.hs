module Telegram.Request.Repeat
  ( sendKeyboardMarkupRequest
  ) where

import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple

import           Telegram.Request

sendKeyboardMarkupRequest ::
     BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
sendKeyboardMarkupRequest userId text token keyboard =
  createRequest
    "POST"
    token
    "sendMessage"
    [ ("text", Just text)
    , ("chat_id", Just userId)
    , ("reply_markup", Just keyboard)
    ]
