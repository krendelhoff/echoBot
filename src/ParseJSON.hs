module ParseJSON
  ( Updates(..)
  , Update(..)
  ) where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics
import           Relude

data Updates =
  Updates
    { ok     :: Bool
    , result :: [Update]
    }
  deriving (Show, Generic, FromJSON)

-- это вся информация для формирования ответного реквеста
data Update =
  Update
    { update_id  :: Int
    , id         :: Int
    , message_id :: Int
    , username   :: Text
    , text       :: Maybe Text
    }
  deriving (Show)

instance FromJSON Update where
  parseJSON =
    withObject "telegram update" $ \o -> do
      update_id <- o .: "update_id"
      messageO <- o .: "message"
      text <- messageO .: "text"
      message_id <- messageO .: "message_id"
      chatO <- messageO .: "chat"
      id <- chatO .: "id"
      username <- chatO .: "username"
      return $ Update {..}
