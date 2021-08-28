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

-- пока что абсолютно независимый логгер делаем
data Handle =
  Handle
    { log :: Priority -> Text -> IO ()
    }

-- так, судя по всему - это интерфейсный файл
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
