module Data.Request
  ( create
  , Info(..)
  , Method
  , Token
  , getUpdates
  , Config(..)
  , parseConfig
  , copyMessage
  ) where

import           Control.Monad.Catch
import           Data.Aeson            (FromJSON)
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

type Method = ByteString

type Token = Text

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
