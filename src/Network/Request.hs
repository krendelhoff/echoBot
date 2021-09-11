module Network.Request
  ( Handle(..)
  , Config(..)
  , Error(..)
  , parseConfig
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

-- why this don't work with ghc 9.0.1?
-- only this: perform :: ExceptT Error IO ByteString
newtype Handle =
  Handle
    { perform :: forall m. (MonadIO m, MonadError Error m, MonadCatch m) =>
                             m ByteString
    }

newtype Config =
  Config
    { token :: Text
    }
  deriving (Show, Generic)

instance FromJSON Config

data Error
  = Code Int
  | Exception Text
  deriving (Show)

parseConfig :: ByteString -> IO Config
parseConfig rawData =
  decodeThrow rawData `catch`
  (\exception -> do
     let errorMsg = showText (exception :: SomeException)
     TIO.putStrLn errorMsg
     TIO.putStrLn "Fatal Error: Request Config no parse"
     exitFailure)
