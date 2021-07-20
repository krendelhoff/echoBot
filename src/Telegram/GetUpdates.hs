module Telegram.GetUpdates
  ( getUpdates
  ) where

import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple

getUpdatesRequest :: BC.ByteString -> Request
getUpdatesRequest offset =
  setRequestQueryString [("timeout", Just "10"), ("offset", Just offset)] $
  setRequestMethod "GET" $
  setRequestSecure True $
  "https://api.telegram.org/bot1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU/getUpdates"

intToByteString :: Int -> BC.ByteString
intToByteString = BC.pack . show

getUpdates :: Int -> ExceptT String IO BC.ByteString
getUpdates offsetValue = do
  response <- liftIO $ httpBS $ getUpdatesRequest (intToByteString offsetValue)
  case getResponseStatusCode response of
    200 -> do
      return (getResponseBody response)
    code -> throwError $ "Request failed with " <> show code <> " code"
