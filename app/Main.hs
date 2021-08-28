module Main where

import           Logger         (Mode (..), Priority (..), log)
import qualified Logger
import qualified Logger.Display
import           Relude
import qualified System.IO      as SIO

main :: IO ()
main = do
  myConfig <- Logger.Display.parseConfig
  Logger.Display.withHandle myConfig $ \hLogger -> do
    log hLogger Error "You are too best!"
-- TODO withHandle должен реализовывать функционал такой, что должен закрываться и handle (файловый)
-- логгер реализован
-- закрывай отдельно
-- отдельно реализовать еще сервис HTTP, полностью независимо, читай статьи
-- теперь вот эти открытые хэндлы наверное можно закинуть в Reader
-- подумать как вынести открытие файла и IHandle в отдельный модуль
