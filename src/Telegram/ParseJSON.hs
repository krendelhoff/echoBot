module Telegram.ParseJSON
  ( parseUpdatesJSON
  , Updates(..)
  , Update(..)
  , Message(..)
  , Chat(..)
  , Sticker(..)
  ) where

import           Control.Monad.Except
import           Data.Aeson
import qualified Data.ByteString.Char8 as BC
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

data Message =
  Message
    { chat    :: Chat
    , text    :: Maybe T.Text
    , sticker :: Maybe Sticker
    , photo   :: Maybe [Sticker]
    , caption :: Maybe Text
    }
  deriving (Show, Generic)

instance FromJSON Message
  {-data Photo =
  Photo
    { file_id :: T.Text
    } deriving (Show, Generic)-}

data Sticker =
  Sticker
    { file_id :: T.Text
    }
  deriving (Show, Generic)

instance FromJSON Sticker

data Chat =
  Chat
    { id       :: Int
    , username :: T.Text
    }
  deriving (Show, Generic)

instance FromJSON Chat

-- непонятно зачем тут IO вообще присобачено к чистому коду, но пока не знаю
-- как избавиться
parseUpdatesJSON :: BC.ByteString -> ExceptT String IO Updates
parseUpdatesJSON updatesJSON = do
  either
    throwError
    return
    (eitherDecodeStrict updatesJSON :: Either String Updates)
