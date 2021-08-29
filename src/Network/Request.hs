module Network.Request
  ( Handle(..)
  , Config(..)
  , Info(..)
  , Error(..)
  , parseConfig
  , create
  ) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Aeson            (FromJSON)
import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as BC (readFile)
import qualified Data.Text.IO          as TIO
import           Data.Yaml             (decodeThrow)
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple
import           Relude                hiding (Handle, log)

import           Miscellanea

data Handle =
  Handle
    { perform :: forall m. (MonadIO m, MonadError Error m, MonadCatch m) =>
                             m ByteString
    }

data Config =
  Config
    { token :: Text
    }
  deriving (Show, Generic)

instance FromJSON Config

data Error
  = Code Int
  | Exception Text
  deriving (Show)

-- здесь не нужно париться, исключение этого вырубает программу
parseConfig :: ByteString -> IO Config
parseConfig rawData =
  decodeThrow rawData `catch`
  (\exception -> do
     let errorMsg = showText (exception :: SomeException)
     TIO.putStrLn errorMsg
     TIO.putStrLn "Fatal Error: Request Config no parse"
     exitFailure)

type Method = ByteString

type Token = Text

create :: Config -> Info -> Request
create Config {..} Info {..} =
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
