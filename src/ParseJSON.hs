module ParseJSON
  ( Updates(..)
  , Update(..)
  , UpdateType(..)
  , Message(..)
  , CallbackQuery(..)
  , CheckSuccessQuery(..)
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
    { update_id :: Int
    , update    :: UpdateType
    }
  deriving (Show)

data Message =
  Message
    { id         :: Int
    , message_id :: Int
    , username   :: Text
    , text       :: Maybe Text
    }
  deriving (Eq, Show)

data CallbackQuery =
  CallbackQuery
    { callback_id   :: Text
    , callback_data :: Text
    , user_id       :: Int
    }
  deriving (Show)

type MessageId = Int

data CheckSuccessQuery
  = CopyMsg Bool MessageId
  | Msg Bool Message
  | Cbq Bool Bool
  deriving (Show)

data UpdateType
  = MessageType Message
  | CallbackQueryType CallbackQuery
  deriving (Show)

instance FromJSON UpdateType where
  parseJSON o =
    asum . map ($ o) $
    [(MessageType <$>) . parseJSON, (CallbackQueryType <$>) . parseJSON]

defaultUpdate = Update 0 (MessageType $ Message 0 0 "" Nothing)

instance FromJSON Message where
  parseJSON =
    withObject "telegram message update" $ \o -> do
      messageO <- o .: "message"
      message_id <- messageO .: "message_id"
      text <- optional $ messageO .: "text"
      chatO <- messageO .: "chat"
      username <- chatO .: "username"
      id <- chatO .: "id"
      return $ Message {..}

instance FromJSON CallbackQuery where
  parseJSON =
    withObject "telegram callback query update" $ \o -> do
      cbO <- o .: "callback_query"
      callback_id <- cbO .: "id"
      callback_data <- cbO .: "data"
      fromO <- cbO .: "from"
      user_id <- fromO .: "id"
      return $ CallbackQuery {..}

instance FromJSON CheckSuccessQuery where
  parseJSON =
    withObject "telegram check success query update" $ \o -> do
      asum . map ($ o) $ [msgParser, copyMsgParser, cbQParser]
    where
      copyMsgParser o = do
        ok <- o .: "ok"
        resultO <- o .: "result"
        message_id <- resultO .: "message_id"
        return $ CopyMsg ok message_id
      cbQParser o = do
        ok <- o .: "ok"
        result <- o .: "result"
        return $ Cbq ok result
      -- repeating myself cause of one small detail
      msgParser o = do
        ok <- o .: "ok"
        messageO <- o .: "result"
        message_id <- messageO .: "message_id"
        text <- optional $ messageO .: "text"
        chatO <- messageO .: "chat"
        username <- chatO .: "username"
        id <- chatO .: "id"
        return $ Msg ok Message {..}

instance FromJSON Update where
  parseJSON =
    withObject "telegram full update parsing" $ \o ->
      Update <$> o .: "update_id" <*> parseJSON (Object o)
