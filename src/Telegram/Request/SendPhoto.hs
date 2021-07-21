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

instance AttemptRequest ([Photo], Text) where
  tryPerformRequest _ _ Nothing = return ()
  tryPerformRequest _ _ (Just ([], _)) = return ()
  tryPerformRequest userid token (Just (photo, caption)) =
    echo userid token (photo, caption)

instance EchoRequest ([Photo], Text) where
  echo userid token ((photo:_), caption) =
    performEchoRequest $
    sendPhotoRequest
      userid
      (encodeUtf8 . fileId $ photo)
      (encodeUtf8 caption)
      token
