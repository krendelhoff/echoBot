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
    { update_id :: Int
    , id        :: Int
    , username  :: Text
    , text      :: Text
    }
  deriving (Show)

instance FromJSON Update where
  parseJSON =
    withObject "telegram update" $ \o -> do
      update_id <- o .: "update_id"
      chatO <- o .: "chat"
      id <- chatO .: "id"
      username <- chatO .: "username"
      text <- o .: "text"
      return $ Update {..}
