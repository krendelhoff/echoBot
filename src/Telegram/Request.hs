module Telegram.Request
  ( createRequest
  , AttemptRequest(..)
  , EchoRequest(..)
  , performEchoRequest
  ) where

import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BC
import           Data.Text             (Text)
import           Data.Text.Encoding    (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.ParseJSON

type HTTPMethod = BC.ByteString

type Token = BC.ByteString

type QueryString = [(BC.ByteString, Maybe BC.ByteString)]

type APIMethod = BC.ByteString

type UserId = BC.ByteString

createRequest :: HTTPMethod -> Token -> APIMethod -> QueryString -> Request
createRequest hmethod token amethod qstr =
  setRequestMethod hmethod $
  setRequestSecure True $
  setRequestPort 443 $
  setRequestHost "api.telegram.org" $
  setRequestPath (mconcat ["/bot", token, "/", amethod]) $
  setRequestQueryString qstr $ defaultRequest

performEchoRequest :: Request -> ExceptT String IO ()
performEchoRequest request = do
  response <- liftIO $ httpBS request
  case getResponseStatusCode response of
    200  -> return ()
    code -> throwError $ "Big problem marked by the " <> show code <> " code"

class EchoRequest a =>
      AttemptRequest a
  where
  tryPerformRequest :: UserId -> Token -> Maybe a -> ExceptT String IO ()

class EchoRequest a where
  echo :: UserId -> Token -> a -> ExceptT String IO ()
