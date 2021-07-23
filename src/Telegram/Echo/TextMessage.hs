module Telegram.Echo.TextMessage
  ( echoTextMessage
  , processTextMessage
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Char8        as BC
import           Data.Map                     (Map)
import qualified Data.Map                     as M
import           Data.Text                    (pack)
import           Data.Text.Encoding           (encodeUtf8)
import           Prelude                      hiding (repeat)

import           Telegram.Configuration
import qualified Telegram.ParseJSON           as PJ
import           Telegram.Request
import           Telegram.Request.SendMessage

processTextMessage ::
     PJ.Message -> StateT (Config, Map Int Int) (ExceptT String IO) ()
processTextMessage m = do
  let userId = encodeUtf8 $ pack $ show $ PJ.id $ PJ.chat m
      text = encodeUtf8 $ PJ.text $ m
  case PJ.text m of
    "/help" -> do
      helpText <- gets getter
      echoTextMessage userId helpText
      where getter (config, _) = encodeUtf8 $ help $ config
    "/repeat" -> do
      rpeat <- gets getter
      echoTextMessage userId ("chosen some repeats")
      where getter (config, map) =
              maybe (accessor $ config) id $ M.lookup map $ PJ.id $ PJ.chat m
              where
                accessor = repeat :: Config -> Int
    _ -> echoTextMessage userId text

echoTextMessage ::
     BC.ByteString
  -> BC.ByteString
  -> StateT (Config, Map Int Int) (ExceptT String IO) ()
echoTextMessage userId text = do
  tokn <- gets getter
  performEchoRequest $ sendMessageRequest userId text tokn
  where
    getter (config, _) = encodeUtf8 $ token $ config
