module Telegram.GetUpdates
  ( getUpdates
  ) where

import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple
import           Telegram.MakeRequest

token :: BC.ByteString
token = "1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU"

getUpdatesRequest :: BC.ByteString -> Request
getUpdatesRequest offset =
  makeRequest
    "GET"
    token
    "getUpdates"
    [("timeout", Just "5"), ("offset", Just offset)]

intToByteString :: Int -> BC.ByteString
intToByteString = BC.pack . show

getUpdates :: Int -> ExceptT String IO BC.ByteString
getUpdates offsetValue = do
  response <- liftIO $ httpBS $ getUpdatesRequest (intToByteString offsetValue)
  case getResponseStatusCode response of
    200 -> do
      return (getResponseBody response)
    code -> throwError $ "Request failed with " <> show code <> " code"
