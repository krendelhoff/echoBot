module Telegram.Configuration
  ( parseConfig
  ) where

import           Control.Monad.Except
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import           Data.Yaml
import           GHC.Generics

data Config =
  Config
    { token   :: T.Text
    , timeout :: Int
    }
  deriving (Show, Generic)

instance FromJSON Config

parseConfig :: ExceptT String IO Config
parseConfig = do
  rawConfig <- liftIO $ B.readFile "config.yaml"
  either (throwError . show) return $ decodeEither' rawConfig
