module Telegram.Echo.Photo
  ( echoPhotoMessage
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (encodeUtf8)

import           Telegram.Configuration
import qualified Telegram.ParseJSON         as PJ
import           Telegram.Request
import           Telegram.Request.SendPhoto

echoPhotoMessage :: PJ.Message -> ReaderT Config (ExceptT String IO) ()
echoPhotoMessage m = do
  let userId = encodeUtf8 $ pack $ show $ PJ.id $ PJ.chat m
      takeFileId = PJ.file_id :: PJ.Photo -> Text
      fileId =
        encodeUtf8 $ takeFileId $ head $ PJ.photo $ m -- FIXME Non-total function
      captn = encodeUtf8 <$> PJ.caption m
  tokn <- asks (encodeUtf8 . token)
  lift $ performEchoRequest $ sendPhotoRequest userId fileId captn tokn
