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

data Handle =
  Handle
    { log :: Priority -> Text -> IO ()
    }

-- может быть сервис может иметь независимый от реализации конфиг, тогда можно попробовать прикрутить его сюда, в интерфейс, но в целом не нужно, как в случае с нашим логгером
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
