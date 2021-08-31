module Data.Request
  ( create
  , Info(..)
  , Method
  , Token
  , getUpdates
  , Config(..)
  , parseConfig
  , copyMessage
  , helpMessage
  , repeatMessage
  , repeatErrorMessage
  , sendMessage
  ) where

import           Control.Monad.Catch
import           Data.Aeson
import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text)
import qualified Data.Text.IO          as TIO
import           Data.Yaml             (decodeThrow)
import           GHC.Generics
import           Network.HTTP.Simple
import           Relude

import           Miscellanea
import qualified Network.Request
import           ParseJSON

data Config =
  Config
    { timeout  :: Int
    , help     :: Text
    , question :: Text
    , repeat   :: Int
    }
  deriving (Generic, Show)

instance FromJSON Config

parseConfig :: ByteString -> IO Config
parseConfig rawData =
  decodeThrow rawData `catch`
  (\exception -> do
     let errorMsg = showText (exception :: SomeException)
     TIO.putStrLn errorMsg
     TIO.putStrLn "Fatal Error: Bot functionality config no parse"
     exitFailure)

create :: Network.Request.Config -> Info -> Request
create Network.Request.Config {..} Info {..} =
  setRequestPath path $
  setRequestMethod "GET" $
  setRequestHost "api.telegram.org" $
  setRequestQueryString qStr $
  setRequestPort 443 $ setRequestSecure True $ defaultRequest
  where
    path = "/bot" <> encodeUtf8 token <> "/" <> method

type Method = ByteString

type Token = Text

data Info =
  Info
    { method :: Method
    , qStr   :: Query
    }
  deriving (Show)

type Offset = Int

type TimeOut = Int

getUpdates :: Offset -> Config -> Info
getUpdates offset funcConfig@Config {..} =
  Info {method = "getUpdates", qStr = getUpdatesQuery offset timeout}
  where
    getUpdatesQuery :: Offset -> TimeOut -> Query
    getUpdatesQuery offset timeout =
      [("offset", Just $ showBS offset), ("timeout", Just $ showBS timeout)]

type ChatId = Int

type MessageId = Int

copyMessage :: ChatId -> MessageId -> Info
copyMessage chat_id message_id =
  Info {method = "copyMessage", qStr = copyMessageQuery chat_id message_id}
  where
    copyMessageQuery :: ChatId -> MessageId -> Query
    copyMessageQuery chat_id message_id =
      ((showBS <$>) <$>) <$>
      [ ("chat_id", Just chat_id)
      , ("from_chat_id", Just chat_id)
      , ("message_id", Just message_id)
      ]

helpMessage :: ChatId -> Config -> Info
helpMessage chat_id Config {..} =
  Info {method = "sendMessage", qStr = helpMessageQuery chat_id help}
  where
    helpMessageQuery chat_id help =
      [("chat_id", Just $ showBS chat_id), ("text", Just $ encodeUtf8 help)]

data Keyboard =
  Keyboard
    { keyboard :: [[Button]]
    }
  deriving (Generic, ToJSON)

data Button =
  Button
    { text :: Text
    }
  deriving (Generic, ToJSON)

repeatMessage :: ChatId -> Config -> Info
repeatMessage chat_id Config {..} =
  Info {method = "sendMessage", qStr = repeatMessageQuery chat_id question}
  where
    repeatMessageQuery chat_id question =
      [ ("chat_id", Just $ showBS chat_id)
      , ("text", Just $ encodeUtf8 question)
      , ("reply_markup", Just $ toStrict $ encode kboard)
      ]
      where
        kboard = Keyboard {keyboard = [map Button ["1", "2", "3", "4", "5"]]}

sendMessage :: ChatId -> Text -> Info
sendMessage chat_id text =
  Info
    { method = "sendMessage"
    , qStr =
        [("chat_id", Just $ showBS chat_id), ("text", Just $ encodeUtf8 text)]
    }

repeatErrorMessage :: ChatId -> Info
repeatErrorMessage chat_id =
  Info
    { method = "sendMessage"
    , qStr =
        [ ("chat_id", Just $ showBS chat_id)
        , ("text", Just $ "Wrong value! Choose from 1 to 5, please.")
        ]
    }
