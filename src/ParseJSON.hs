module ParseJSON
  ( Updates(..)
  , Update(..)
  , defaultUpdate
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

defaultUpdate = Update 0 0 0 "" Nothing

instance FromJSON Update where
  parseJSON =
    withObject
      "telegram update"
      ((<|>) <$> updateParser <*> copyMessageCheckSuccessParser)
    where
      updateParser o = do
        update_id <- o .: "update_id"
        messageO <- o .: "message"
        text <- optional $ messageO .: "text"
        message_id <- messageO .: "message_id"
        chatO <- messageO .: "chat"
        id <- chatO .: "id"
        username <- chatO .: "username"
        return $ Update {..}
      copyMessageCheckSuccessParser o = do
        let update_id = 0
            id = 0
            username = ""
            text = Nothing
        message_id <- o .: "message_id"
        return $ Update {..}
