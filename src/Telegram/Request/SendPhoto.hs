module Telegram.Request.SendPhoto
  ( sendPhotoRequest
  ) where

import qualified Data.ByteString.Char8 as BC
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.ParseJSON
import           Telegram.Request

sendPhotoRequest ::
     BC.ByteString -> BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
sendPhotoRequest chat_id photo caption token =
  createRequest
    "POST"
    token
    "sendPhoto"
    [ ("chat_id", Just chat_id)
    , ("caption", Just caption)
    , ("photo", Just photo)
    ]
