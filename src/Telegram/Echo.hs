module Telegram.Echo
  ( echoBot
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Bitraversable           (bisequence)
import qualified Data.ByteString.Char8        as BC
import           Data.Text                    (Text)
import           Data.Text.Encoding           (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.Configuration
import qualified Telegram.ParseJSON           as PJ
import           Telegram.Request
import           Telegram.Request.SendMessage
import           Telegram.Request.SendPhoto
import           Telegram.Request.SendSticker

echoBot :: PJ.Updates -> ReaderT Config (ExceptT String IO) ()
echoBot updates = do
  tokenValue <- asks (encodeUtf8 . token)
  forM_ (PJ.result updates) (\update -> undefined)
