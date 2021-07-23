module Telegram.Configuration
  ( parseConfig
  , Config(..)
  ) where

import           Control.Monad.Except
import qualified Data.ByteString      as B
import           Data.Text            (Text)
import           Data.Yaml
import           GHC.Generics

import           Telegram.Log

data Config =
  Config
    { token   :: Text
    , timeout :: Int
    , logMode :: LogLevel
    , repeat  :: Int
    , help    :: Text
    }
  deriving (Show, Generic)

instance FromJSON Config

parseConfig :: ExceptT String IO Config
parseConfig = do
  rawConfig <- liftIO $ B.readFile "config.yaml"
  either (throwError . show) return $ decodeEither' rawConfig
