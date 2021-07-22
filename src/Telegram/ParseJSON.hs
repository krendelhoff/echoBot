module Telegram.ParseJSON
  ( parseUpdatesJSON
  , Updates(..)
  , Update(..)
  , Message(..)
  , Photo(..)
  , Sticker(..)
  ) where

import           Control.Applicative
import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Char8 as BC
import           Data.Text             (Text)
import qualified Data.Text             as T
import           GHC.Generics

data Updates =
  Updates
    { ok     :: Bool
    , result :: [Update]
    }
  deriving (Show, Generic)

instance FromJSON Updates

data Update =
  Update
    { update_id :: Int
    , message   :: Message
    }
  deriving (Show, Generic)

instance FromJSON Update

data Message
  = TextMessage
      { chat :: Chat
      , text :: Text
      }
  | PhotoMessage
      { chat    :: Chat
      , photo   :: [Photo]
      , caption :: Maybe Text
      }
  | StickerMessage
      { chat    :: Chat
      , sticker :: Sticker
      }
  deriving (Show, Generic)

instance FromJSON Message where
  parseJSON (Object v) =
    (TextMessage <$> v .: "chat" <*> v .: "text") <|>
    (StickerMessage <$> v .: "chat" <*> v .: "sticker") <|>
    (PhotoMessage <$> v .: "chat" <*> v .: "photo" <*>
     (return <$> (v .: "caption"))) <|>
    (PhotoMessage <$> v .: "chat" <*> v .: "photo" <*> (pure Nothing))

data Chat =
  Chat
    { id       :: Int
    , username :: Text
    }
  deriving (Show, Generic)

instance FromJSON Chat

data Photo =
  Photo
    { file_id :: Text
    }
  deriving (Show, Generic)

instance FromJSON Photo

data Sticker =
  Sticker
    { file_id :: Text
    }
  deriving (Show, Generic)

instance FromJSON Sticker

-- непонятно зачем тут IO вообще присобачено к чистому коду, но пока не знаю
-- как избавиться
parseUpdatesJSON :: BC.ByteString -> ExceptT String IO Updates
parseUpdatesJSON updatesJSON = do
  either
    throwError
    return
    (eitherDecodeStrict updatesJSON :: Either String Updates)
