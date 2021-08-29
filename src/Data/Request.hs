module Data.Request
  ( create
  , Info(..)
  , Method
  , Token
  , getUpdatesQuery
  , getUpdates
  , Config(..)
  , parseConfig
  ) where

import           Control.Monad.Catch
import           Data.Aeson            (FromJSON)
import           Data.ByteString.Char8 (ByteString)
import           Data.Text             (Text)
import qualified Data.Text.IO          as TIO
import           Data.Yaml             (decodeThrow)
import           GHC.Generics
import           Miscellanea
import           Network.HTTP.Simple
import           Relude

import qualified Network.Request

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

getUpdatesQuery :: Offset -> TimeOut -> Query
getUpdatesQuery offset timeout =
  [("offset", Just $ showBS offset), ("timeout", Just $ showBS timeout)]

getUpdates :: Offset -> Config -> Info
getUpdates offset funcConfig@Config {..} =
  Info {method = "getUpdates", qStr = getUpdatesQuery offset timeout}
