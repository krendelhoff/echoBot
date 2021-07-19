module ParseJSON
  ( parseUpdatesJSON
  , Updates(..)
  , Update(..)
  , Message(..)
  , Chat(..)
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
    { chat :: Chat
    , text :: T.Text
    }
  deriving (Show, Generic)

instance FromJSON Message

data Chat =
  Chat
    { id       :: Int
    , username :: T.Text
    }
  deriving (Show, Generic)

instance FromJSON Chat

parseUpdatesJSON :: BC.ByteString -> ExceptT String IO Updates
parseUpdatesJSON updatesJSON = do
  either
    throwError
    return
    (eitherDecodeStrict updatesJSON :: Either String Updates)
