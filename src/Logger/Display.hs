module Logger.Display
  ( IHandle(..)
  , Config(..)
  , parseConfig
  , new
  , close
  , withHandle
  , log
  ) where

import           Control.Monad.Catch
import           Data.Aeson            (FromJSON)
import           Data.ByteString.Char8 (ByteString)
import           Data.Foldable         (traverse_)
import           Data.Text             (Text, pack)
import qualified Data.Text.IO          as TIO (hPutStrLn, putStrLn)
import           Data.Yaml             (decodeThrow)
import           GHC.Generics
import           Relude
import           System.IO             (IOMode (AppendMode), hClose, openFile,
                                        stdout)
import qualified System.IO             as SIO (Handle)

import           Logger                (Mode (..))
import qualified Logger
import           Miscellanea

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
  deriving (Eq, Show, Generic)

instance FromJSON Config

parseConfig :: ByteString -> IO Config
parseConfig rawData =
  decodeThrow rawData `catch`
  (\exception -> do
     let errorMsg = showText (exception :: SomeException)
     -- может стоит написать функцию переводящую в текст
     TIO.putStrLn errorMsg
     TIO.putStrLn "Fatal Error: Logger config no parse"
     exitFailure)

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

-- в этом и есть глубинная суть судя по всему - implemetation Handle inside interface Handle
-- имплементацию прячем в интерфейсные функции, очень логично
-- это стало очевидно, когда я вызвал withIHandle myConfig $ \ih ->
--                                  withHandle myConfig ih $ \hLogger -> ..
withHandle :: Config -> (Logger.Handle -> IO a) -> IO a
withHandle config f =
  withIHandle config $ \ih -> bracket (new config ih) close f

log :: IHandle -> Logger.Priority -> Text -> IO ()
log IHandle {..} prior txt
  | prior > Logger.None && prior <= priority config =
    traverse_ ((flip TIO.hPutStrLn) logMsg) handles
  | otherwise = return ()
  where
    logMsg = showText prior <> ": " <> txt
    -- ^ better to use fmt, but i can't cause packet restriction
