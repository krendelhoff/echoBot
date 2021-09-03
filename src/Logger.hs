module Logger
  ( Handle(..)
  , Mode(..)
  , Priority(..)
  ) where

import           Data.Aeson
import           Data.Text    (Text)
import           GHC.Generics
import           Relude       hiding (Handle)
import qualified System.IO    (Handle)

newtype Handle =
  Handle
    { log :: Priority -> Text -> IO ()
    }

data Mode
  = Stdout
  | File
  | Both
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Mode

data Priority
  = None
  | Debug
  | Error
  | Warn
  | Info
  deriving (Eq, Ord, Show, Generic)

instance FromJSON Priority
