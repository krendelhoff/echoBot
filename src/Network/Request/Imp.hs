module Network.Request.Imp
  ( Handle
  , newHandle
  , closeHandle
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
import qualified Network.Request       as Request

-- implementation Handle
data Handle =
  Handle
    { config  :: Request.Config
    , hLogger :: Logger.Handle
    , info    :: Request.Info
    }

newHandle :: Request.Config -> Logger.Handle -> Request.Info -> IO Handle
newHandle config hLogger = return . Handle config hLogger

closeHandle :: Handle -> IO ()
closeHandle _ = return ()

withIHandle ::
     Request.Config -> Logger.Handle -> Request.Info -> (Handle -> IO a) -> IO a
withIHandle config hLogger reqInfo =
  bracket (newHandle config hLogger reqInfo) closeHandle

new :: Handle -> IO Request.Handle
new hRequest = do
  return $ Request.Handle {perform = perform hRequest}

close :: Request.Handle -> IO ()
close _ = return ()

withHandle ::
     Request.Config
  -> Logger.Handle
  -> Request.Info
  -> (Request.Handle -> IO a)
  -> IO a
withHandle config hLogger reqInfo f =
  withIHandle config hLogger reqInfo $ \ih -> do bracket (new ih) close f

perform ::
     (MonadCatch m, MonadError Request.Error m, MonadIO m)
  => Handle
  -> m ByteString
perform Handle {..} =
  (do let req = Request.create config info
      response <- liftIO $ httpBS req
      case getResponseStatusCode response of
        200 -> do
          let method = decodeUtf8 $ Request.method info
          liftIO $ log hLogger Info (method <> " request made successfully")
          return $ getResponseBody response
        x -> do
          let errorMsg = showText x
          liftIO $ log hLogger Error errorMsg
          throwError $ Request.Code x) `catch`
  (\exception -> do
     let errorMsg = showText (exception :: SomeException)
     liftIO $ log hLogger Error errorMsg
     throwError $ Request.Exception errorMsg)
