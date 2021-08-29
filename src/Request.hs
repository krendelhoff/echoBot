module Request
  ( Config(..)
  , parseConfig
  , create
  , perform
  , new
  , close
  , withHandle
  ) where

import           Control.Exception     hiding (bracket, catch)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Aeson            (FromJSON)
import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as BC (readFile)
import           Data.IORef            (IORef)
import           Data.List.NonEmpty    (NonEmpty ((:|)))
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Yaml             (decodeThrow)
import           GHC.Generics          (Generic)
import           Network.HTTP.Conduit  (path)
import           Network.HTTP.Simple
import           Relude                hiding (Handle, log)

import           Logger                (Priority (..), log)
import qualified Logger
import qualified Logger.Display
import           Miscellanea

data Config =
  Config
    { token    :: Text
    , repeat   :: Int
    , help     :: Text
    , question :: Text
    , timeout  :: Int
    }
  deriving (Show, Generic)

instance FromJSON Config

-- implementation Handle
data Handle =
  Handle
    { config    :: Config
    , hLogger   :: Logger.Handle
    , req       :: Request
    , offsetRef :: IORef Int
    }

new :: Config -> Logger.Handle -> Request -> Maybe Int -> IO Handle
new config@Config {..} hLogger req mOffset =
  fmap (Handle config hLogger req) (newIORef $ maybe 0 id mOffset)

close :: Handle -> IO ()
close _ = return ()

withHandle ::
     Config -> Logger.Handle -> Request -> Maybe Int -> (Handle -> IO a) -> IO a
withHandle config hLogger req mOffset =
  bracket (new config hLogger req mOffset) close

data RequestError
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

create :: Token -> Method -> Query -> Request
create token method qs =
  setRequestPath path $
  setRequestMethod "GET" $
  setRequestHost "api.telegram.org" $
  setRequestQueryString qs $
  setRequestPort 443 $ setRequestSecure True $ defaultRequest
  where
    path = "/bot" <> encodeUtf8 token <> "/" <> method

-- если тут мы получили любое исключение, программа жива и просто идет на следующую итерацию
-- качественный код!!!
-- TODO как тестировать эту функцию?
perform ::
     (MonadCatch m, MonadError RequestError m, MonadIO m)
  => Handle
  -> m ByteString
perform Handle {..} =
  (do response <- liftIO $ httpBS req
      case getResponseStatusCode response of
        200 -> do
          let method = getAPIMethod req
          liftIO $ log hLogger Info (method <> " request made successfully")
          return $ getResponseBody response
        x -> do
          let errorMsg = showText x
          liftIO $ log hLogger Error errorMsg
          throwError $ Code x) `catch`
  (\exception -> do
     let errorMsg = showText (exception :: SomeException)
     liftIO $ log hLogger Error errorMsg
     throwError $ Exception errorMsg)
  where
    getAPIMethod req = last $ "" :| T.splitOn "/" (decodeUtf8 $ path req)
    -- non-total decodeUtf8, but totally safe here
