module Telegram.Request
  ( createRequest
  , performEchoRequest
  , performCommandRequest
  , Token
  ) where

import           Control.Monad          (replicateM_)
import           Control.Monad.Except
import           Control.Monad.State
import qualified Data.ByteString.Char8  as BC
import           Data.Map               (Map)
import qualified Data.Map               as M
import           Data.Text              (Text)
import           Data.Text.Encoding     (encodeUtf8)
import           Network.HTTP.Simple
import           Prelude                hiding (repeat)

import           Telegram.Configuration
import           Telegram.Log
import           Telegram.Log.Error
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

-- TODO реализовать Validate: даже если какой-то реквес не удался, совершить попроббовать надо все, прологгировать те, с какими была совершена ошибка
performEchoRequest ::
     Request -> Int -> StateT (Config, Map Int Int) (ExceptT String IO) ()
performEchoRequest request userId = tryPerformEchoRequest `catchError` logError
  where
    tryPerformEchoRequest = do
      rep <- gets getter
      replicateM_ rep requestAction
      where
        getter (config, map) = maybe (repeat $ config) id $ M.lookup userId map
    requestAction = do
      response <- liftIO $ httpBS request
      case getResponseStatusCode response of
        200 -> return ()
        code ->
          throwError $
          "Big problem marked by the " <> show code <> " code doing echo"
    logError err = do
      liftIO $ writeLog ERROR err
      throwError err

performCommandRequest ::
     Request -> Int -> StateT (Config, Map Int Int) (ExceptT String IO) ()
performCommandRequest request userId =
  tryPerformCommandRequest `catchError` logError
  where
    tryPerformCommandRequest = do
      requestAction
    requestAction = do
      response <- liftIO $ httpBS request
      case getResponseStatusCode response of
        200 -> return ()
        code ->
          throwError $
          "Big problem marked by the " <> show code <> " code doing command"
