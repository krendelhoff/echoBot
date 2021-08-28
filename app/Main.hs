module Main where

import           Logger         (Mode (..), Priority (..), log)
import qualified Logger
import qualified Logger.Display
import qualified System.IO      as SIO

main :: IO ()
main = do
  let myConfig =
        Logger.Display.Config {mode = Both, priority = Info, file = "log.txt"}
  Logger.Display.withIHandle myConfig $ \ih -> do
    Logger.Display.withHandle myConfig ih $ \hLogger -> do
      log hLogger Warn "biba - boba!!!!!!!!!!!!!!"
-- TODO withHandle должен реализовывать функционал такой, что должен закрываться и handle (файловый)
-- логгер реализован
-- закрывай отдельно
-- отдельно реализовать еще сервис HTTP, полностью независимо, читай статьи
-- теперь вот эти открытые хэндлы наверное можно закинуть в Reader
-- подумать как вынести открытие файла и IHandle в отдельный модуль
