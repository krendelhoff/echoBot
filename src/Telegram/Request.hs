module Telegram.Request
  ( createRequest
  , Token
  ) where

import qualified Data.ByteString.Char8  as BC
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Network.HTTP.Simple

import           Telegram.Configuration
import qualified Telegram.ParseJSON     as PJ

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
