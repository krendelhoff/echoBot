module Telegram.Configuration
  ( parseConfig
  , Config(..)
  , UserRepeat
  ) where

import           Control.Monad.Except
import qualified Data.ByteString      as B
import           Data.Map             (Map)
import           Data.Text            (Text)
import           Data.Yaml
import           GHC.Generics

import           Telegram.Log

type UserRepeat = Map Int Int

data Config =
  Config
    { token          :: Text
    , timeout        :: Int
    , logMode        :: LogLevel
    , repeat         :: Int
    , repeatQuestion :: Text
    , help           :: Text
    , logFile        :: String
    , messager       :: Text
    }
  deriving (Show, Generic)

instance FromJSON Config

parseConfig :: (MonadError String m, MonadIO m) => m Config
parseConfig = do
  rawConfig <- liftIO $ B.readFile "config.yaml"
  either (throwError . show) return $ decodeEither' rawConfig
