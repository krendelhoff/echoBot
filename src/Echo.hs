module Echo
  ( echo
  ) where

import           Control.Monad.Except
import qualified Data.ByteString.Char8 as BC
import           Data.Text.Encoding    (encodeUtf8)
import           Network.HTTP.Simple

import qualified ParseJSON             as PJ

-- вспомнить про то как используя ExceptT накапливать все ошибки, а не останавливаться на первой(stepik)
sendMessageRequest :: BC.ByteString -> BC.ByteString -> Request
sendMessageRequest idValue textValue =
  setRequestQueryString [("chat_id", Just idValue), ("text", Just textValue)] $
  setRequestMethod "POST" $
  setRequestSecure True $
  "https://api.telegram.org/bot1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU/sendMessage"

echo :: PJ.Updates -> ExceptT String IO ()
echo updates = do
  forM_
    (PJ.result updates)
    (\update -> do
       let textMessage = PJ.text $ PJ.message $ update
           idValue = BC.pack $ show (PJ.id $ PJ.chat $ PJ.message $ update)
       response <-
         liftIO $ httpBS $ sendMessageRequest idValue (encodeUtf8 textMessage)
       case getResponseStatusCode response of
         200 -> return ()
         code ->
           throwError $ "Big problem marked by the " <> show code <> " code")
