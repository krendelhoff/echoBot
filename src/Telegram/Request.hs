module Telegram.Request
  ( createRequest
  ) where

import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple

type HTTPMethod = BC.ByteString

type Token = BC.ByteString

type QueryString = [(BC.ByteString, Maybe BC.ByteString)]

type APIMethod = BC.ByteString

createRequest :: HTTPMethod -> Token -> APIMethod -> QueryString -> Request
createRequest hmethod token amethod qstr =
  setRequestMethod hmethod $
  setRequestSecure True $
  setRequestPort 443 $
  setRequestHost "api.telegram.org" $
  setRequestPath (mconcat ["/bot", token, "/", amethod]) $
  setRequestQueryString qstr $ defaultRequest
