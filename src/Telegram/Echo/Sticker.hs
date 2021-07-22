module Telegram.Echo.Sticker
  ( echoSticker
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text                    (Text, pack)
import           Data.Text.Encoding           (encodeUtf8)

import           Telegram.Configuration
import qualified Telegram.ParseJSON           as PJ
import           Telegram.Request
import           Telegram.Request.SendSticker

echoSticker :: PJ.Message -> ReaderT Config (ExceptT String IO) ()
echoSticker m = do
  let userId = encodeUtf8 $ pack $ show $ PJ.id $ PJ.chat m
      takeFileId = PJ.file_id :: PJ.Sticker -> Text
      file_id = encodeUtf8 $ takeFileId $ PJ.sticker $ m
  tokn <- asks (encodeUtf8 . token)
  lift $ performEchoRequest $ sendStickerRequest userId file_id tokn
