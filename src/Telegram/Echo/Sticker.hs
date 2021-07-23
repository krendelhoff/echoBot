module Telegram.Echo.Sticker
  ( echoSticker
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Map                     (Map)
import           Data.Text                    (Text, pack)
import           Data.Text.Encoding           (encodeUtf8)

import           Telegram.Configuration
import qualified Telegram.ParseJSON           as PJ
import           Telegram.Request
import           Telegram.Request.SendSticker

echoSticker :: PJ.Message -> StateT (Config, Map Int Int) (ExceptT String IO) ()
echoSticker m = do
  let userId = encodeUtf8 $ pack $ show $ PJ.id $ PJ.chat m
      takeFileId = PJ.file_id :: PJ.Sticker -> Text
      file_id = encodeUtf8 $ takeFileId $ PJ.sticker $ m
  tokn <- gets getter
  performEchoRequest
    (sendStickerRequest userId file_id tokn)
    (PJ.id $ PJ.chat $ m)
  where
    getter (config, _) = encodeUtf8 $ token $ config
