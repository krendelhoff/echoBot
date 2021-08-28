module Request
  ( parseConfig
  , create
  ) where

import           Control.Exception     hiding (catch)
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Aeson            (FromJSON)
import           Data.ByteString.Char8 (ByteString, pack)
import qualified Data.ByteString.Char8 as BC (readFile)
import           Data.IORef            (IORef)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Yaml             (decodeThrow)
import           GHC.Generics          (Generic)
import           Network.HTTP.Simple
import           Relude                hiding (Handle, log)

import           Logger                (Priority (..), log)
import qualified Logger
import qualified Logger.Display

-- по сути квалифайд нигде не нужен, просто если возникает ambigous, то везде нужно явно указывать откуда
-- тут реализация 100% будет одна - соединим интерфейс и реализацию
-- да и интерфейс мне неочевиден - нужно сначала хотя бы реализацию
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
    { offsetRef :: IORef Int
    , config    :: Config
    , hLogger   :: Logger.Handle
    }

data RequestError
  = Code Int
  | Dirty Text

-- здесь не нужно париться, исключение этого вырубает программу
parseConfig :: IO Config
parseConfig = BC.readFile "config.yaml" >>= decodeThrow

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
getUpdates ::
     (MonadCatch m, MonadError RequestError m, MonadIO m)
  => Handle
  -> m ByteString
getUpdates Handle {..} =
  (do offsetValue <- readIORef offsetRef
      let queryOffset = pack . show $ offsetValue
          req =
            create (token config) "/getUpdates" [("offset", Just queryOffset)]
      response <- liftIO $ httpBS req
      case getResponseStatusCode response of
        200 -> do
          liftIO $ log hLogger Info "getUpdates request made successfully"
          return $ getResponseBody response
        x -> do
          let errorMsg = T.pack $ show $ x
          liftIO $ log hLogger Error errorMsg
          throwError $ Code x) `catch`
  (\exception -> do
     let errorMsg = T.pack $ show $ exception
     liftIO $ log hLogger Error errorMsg
     throwError $ Dirty errorMsg)
