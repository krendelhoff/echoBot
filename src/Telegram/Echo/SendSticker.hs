module Telegram.Echo.SendSticker
  ( sendStickerRequest
  ) where

import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple

sendStickerRequest :: BC.ByteString -> BC.ByteString -> Request
sendStickerRequest chat_id sticker =
  setRequestQueryString [("chat_id", Just chat_id), ("sticker", Just sticker)] $
  setRequestMethod "POST" $
  setRequestSecure True $
  "https://api.telegram.org/bot1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU/sendSticker"
