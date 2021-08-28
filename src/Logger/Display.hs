module Logger.Display
  ( IHandle(..)
  , Config(..)
  , parseConfig
  , new
  , close
  , withHandle
  , newIHandle
  , closeIHandle
  , withIHandle
  , log
  ) where

import           Control.Exception (bracket)
import           Data.Foldable     (traverse_)
import           Data.Text         (Text, pack)
import qualified Data.Text.IO      as TIO (hPutStrLn)
import           Logger            (Mode (..))
import qualified Logger
import           System.IO         (IOMode (AppendMode), hClose, openFile,
                                    stdout)
import qualified System.IO         as SIO (Handle)

data IHandle =
  IHandle
    { config  :: Config
    , handles :: [SIO.Handle]
    }

data Config =
  Config
    { priority :: Logger.Priority
    , mode     :: Logger.Mode
    , file     :: FilePath
    }

parseLoggerC :: IO Config
parseLoggerC = undefined

-- вот это всё можно засунуть в "сервис", отвечающий именно за открытие потоков/файлов и сделать логгер зависимым от него
newIHandle :: Config -> IO IHandle
newIHandle c@Config {..} =
  case mode of
    Stdout -> return $ IHandle c [stdout]
    otherwise -> do
      hFile <- openFile file AppendMode
      -- TODO отловить возможное исключение
      case mode of
        File -> return $ IHandle c [hFile]
        Both -> return $ IHandle c [hFile, stdout]

closeIHandle :: IHandle -> IO ()
closeIHandle IHandle {..} = traverse_ hClose . filter (/= stdout) $ handles

withIHandle :: Config -> (IHandle -> IO a) -> IO a
withIHandle c f = bracket (newIHandle c) closeIHandle f

-- IHandle is dep
new :: Config -> IHandle -> IO (Logger.Handle)
new _ ih = return $ Logger.Handle {Logger.log = log ih}

close :: Logger.Handle -> IO ()
close _ = return ()

withHandle :: Config -> IHandle -> (Logger.Handle -> IO a) -> IO a
withHandle config ih f = bracket (new config ih) close f

log :: IHandle -> Logger.Priority -> Text -> IO ()
log IHandle {..} prior txt
  | prior > Logger.None && prior <= priority config =
    traverse_ ((flip TIO.hPutStrLn) logMsg) handles
  | otherwise = return ()
  where
    logMsg = (pack . show) prior <> ": " <> txt
    -- ^ better to use fmt, but i can't cause packet restriction
