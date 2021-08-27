module Logger
  ( LogLevel(..)
  , LogMode(..)
  , Config(..)
  , parseConfig
  , Handle
  , new
  , close
  , withHandle
  , log
  ) where

import           System.IO hiding (Handle)
import qualified System.IO (Handle)

-- пока что абсолютно независимый логгер делаем
data Handle =
  Handle
    { file :: Maybe (System.IO.Handle)
    , log  :: IO ()
    }

-- так, судя по всему - это интерфейсный файл
-- теперь может быть 3 реализации
data LogMode
  = Stdout
  | File
  | Both
  deriving (Eq, Ord, Show)

data LogLevel
  = None
  | Debug
  | Error
  | Warn
  | Info

data Config =
  Config
    { file :: Maybe FilePath
    , mode :: LogMode
    , lvl  :: LogLevel
    }

parseConfig :: MonadIO m => m Config
parseConfig = undefined

new :: Config -> IO Handle
new config =
  if mode > Stdout
    then undefined
    else undefined

close :: Handle -> IO ()
close = return ()

withHandle :: Config -> (Handle -> IO a) -> IO a
withHandle config f = bracket (new config) close f
