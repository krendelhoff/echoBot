module Logger.Display where
  -- TODO надо ограничить экспорт

import           Control.Exception (bracket)
import           Data.Text         (Text, pack)
import qualified Data.Text.IO      as TIO (hPutStrLn)
import qualified Logger
import qualified System.IO         as SIO (Handle, openFile, stdout)

data IHandle =
  IHandle
    { config :: Config
    , handle :: SIO.Handle
    }

data Config =
  Config
    { priority :: Logger.Priority
    , mode     :: Logger.Mode
    , file     :: Maybe FilePath
    }

-- IHandle is dep
new :: Config -> IHandle -> IO (Logger.Handle)
new _ ih = return $ Logger.Handle {Logger.log = log ih}

-- but here should be no IHandle according to article...think
close :: Logger.Handle -> IO ()
close _ = return ()

withHandle :: Config -> IHandle -> (Logger.Handle -> IO a) -> IO a
withHandle config ih f = bracket (new config ih) close f

-- TODO RecordWildCards
log :: IHandle -> Text -> IO ()
log ih txt =
  TIO.hPutStrLn
    (handle ih)
    ((pack $ show $ priority $ config $ ih) <> ": " <> txt)
