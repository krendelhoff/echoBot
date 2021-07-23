module Telegram.Request.GetUpdates
  ( getUpdates
  ) where

import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Char8  as BC
import           Data.Map               (Map)
import           Data.Text.Encoding     (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.Configuration
import           Telegram.Log
import           Telegram.Log.Error
import           Telegram.Request

getUpdatesRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
getUpdatesRequest offset token timeout =
  createRequest
    "GET"
    token
    "getUpdates"
    [("timeout", Just timeout), ("offset", Just offset)]

getUpdates ::
     BC.ByteString
  -> StateT (Config, Map Int Int) (ExceptT String IO) BC.ByteString
getUpdates offst = tryGetUpdates `catchError` logError
  where
    tryGetUpdates = do
      (tokn, timeut) <- gets getter
      response <- liftIO $ httpBS $ getUpdatesRequest offst tokn timeut
      case getResponseStatusCode response of
        200  -> return (getResponseBody response)
        code -> throwError $ "Request failed with " <> show code <> " code"
      where
        getter (config, _) =
          (encodeUtf8 $ token config, BC.pack $ show $ timeout config)
