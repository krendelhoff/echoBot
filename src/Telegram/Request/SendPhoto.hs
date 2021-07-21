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
    "sendSticker"
    [ ("chat_id", Just chat_id)
    , ("caption", Just caption)
    , ("photo", Just photo)
    ]

instance AttemptRequest (Photo, Text) where
  tryPerformRequest _ _ Nothing                 = return ()
  tryPerformRequest userid token (Just sticker) = echo userid token sticker

instance EchoRequest (Photo, Text) where
  echo userid token sticker =
    performEchoRequest $
    sendStickerRequest userid (encodeUtf8 . file_id $ sticker) token
