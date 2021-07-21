module Telegram.Echo
  ( echo
  , debugRequest
  ) where

import           Control.Monad.Except
import qualified Data.ByteString.Char8        as BC
import           Data.Text.Encoding           (encodeUtf8)
import           Network.HTTP.Simple

import qualified Telegram.ParseJSON           as PJ
import           Telegram.Request.SendMessage
import           Telegram.Request.SendSticker

debugRequest :: PJ.Update -> Request -> ExceptT String IO ()
debugRequest update _ = undefined {-do
  let (Just sticker) = PJ.sticker $ PJ.message $ update
      fileId = PJ.file_id sticker
      idValue = BC.pack $ show (PJ.id $ PJ.chat $ PJ.message $ update)
  response <- liftIO $ httpBS $ sendStickerRequest idValue (encodeUtf8 fileId)
  case getResponseStatusCode response of
    200  -> return ()
    code -> throwError $ "Big problem marked by the " <> show code <> " code"
    -}

performRequest :: Request -> ExceptT String IO ()
performRequest request = do
  response <- liftIO $ httpBS request
  case getResponseStatusCode response of
    200  -> return ()
    code -> throwError $ "Big problem marked by the " <> show code <> " code"

echo :: PJ.Updates -> ExceptT String IO ()
echo updates = undefined {-do
  forM_
    (PJ.result updates)
    (\update -> do
       let idValue = BC.pack $ show (PJ.id $ PJ.chat $ PJ.message $ update)
           textMessage = PJ.text $ PJ.message $ update
           sticker = PJ.sticker $ PJ.message $ update
       {-tryMakeRequest textMessage
       tryMakeRequest sticker :: TODO typeclass -}
       maybe
         (return ())
         (\text -> do makeRequest $ sendMessageRequest idValue (encodeUtf8 text))
         textMessage
       maybe
         (return ())
         (\sticker -> do
            makeRequest $
              sendStickerRequest idValue (encodeUtf8 . PJ.file_id $ sticker))
         sticker)-}
