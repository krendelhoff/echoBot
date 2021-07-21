module Telegram.Request.GetUpdates
  ( getUpdates
  ) where

import           Control.Monad.Except
import           Control.Monad.Reader
import qualified Data.ByteString.Char8  as BC
import           Data.Text.Encoding     (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.Configuration
import           Telegram.Request

getUpdatesRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
getUpdatesRequest offset token timeout =
  createRequest
    "GET"
    token
    "getUpdates"
    [("timeout", Just timeout), ("offset", Just offset)]

intToByteString :: Int -> BC.ByteString
intToByteString = BC.pack . show

getUpdates :: Int -> ReaderT Config (ExceptT String IO) BC.ByteString
getUpdates offsetValue = do
  config <- ask
  response <-
    liftIO $
    httpBS $
    getUpdatesRequest
      (intToByteString offsetValue)
      (encodeUtf8 . token $ config)
      (intToByteString $ timeout config)
  case getResponseStatusCode response of
    200 -> do
      return (getResponseBody response)
    code -> throwError $ "Request failed with " <> show code <> " code"
