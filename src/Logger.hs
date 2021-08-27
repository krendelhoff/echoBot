module Logger where

import           Data.Text (Text)
import qualified System.IO (Handle)

-- пока что абсолютно независимый логгер делаем
data Handle =
  Handle
    { log :: Text -> IO ()
    }

-- так, судя по всему - это интерфейсный файл
data Mode
  = Stdout
  | File
  | Both
  deriving (Eq, Ord, Show)

data Priority
  = None
  | Debug
  | Error
  | Warn
  | Info
  deriving (Eq, Ord, Show)
