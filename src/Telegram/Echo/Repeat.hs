module Telegram.Echo.Repeat
  ( performRepeatChange
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Aeson
import qualified Data.ByteString.Char8      as BC
import           Data.ByteString.Lazy.Char8 (toStrict)
import           Data.Map                   (Map)
import qualified Data.Map                   as M
import           Data.Text                  (Text)
import           Data.Text.Encoding         (encodeUtf8)
import           GHC.Generics
import           Network.HTTP.Simple
import           Prelude                    hiding (repeat)

import           Telegram.Configuration
import           Telegram.Log.Error
import qualified Telegram.ParseJSON         as PJ
import           Telegram.Request
import           Telegram.Request.Repeat

data Keyboard =
  Keyboard
    { inline_keyboard :: [[Button]]
    }
  deriving (Show, Generic)

instance ToJSON Keyboard

data Button =
  Button
    { text          :: Text
    , callback_data :: Text
    }
  deriving (Show, Generic)

instance ToJSON Button

performRepeatChange ::
     PJ.Message -> StateT (Config, Map Int Int) (ExceptT String IO) ()
performRepeatChange m = tryPerformRepeatChange `catchError` logError
  where
    tryPerformRepeatChange = do
      (tokn, rpeatQ, rpeat) <- gets getter
      let userId = BC.pack $ show $ PJ.id $ PJ.chat $ m
      performCommandRequest
        (sendKeyboardMarkupRequest
           userId
           (rpeatQ <> ": " <> rpeat)
           tokn
           keyboard)
      where
        getter (config, map) =
          ( encodeUtf8 $ token $ config
          , encodeUtf8 $ repeatQuestion $ config
          , BC.pack $
            show $
            maybe (repeat $ config) id $ M.lookup (PJ.id $ PJ.chat $ m) map)
        keyboard =
          toStrict $
          encode $
          Keyboard
            [ [ Button "1" "1"
              , Button "2" "2"
              , Button "3" "3"
              , Button "4" "4"
              , Button "5" "5"
              ]
            ]
