module Main where

import           Logger                (Mode (..), Priority (..), log)
import qualified Logger
import qualified Logger.Display
import qualified Request

import qualified Data.ByteString.Char8 as BC (readFile)
import           Relude
import qualified System.IO             as SIO

main :: IO ()
main = do
  rawConfig <- BC.readFile "config.yaml"
  cLogger <- Logger.Display.parseConfig rawConfig
  cRequest <- Request.parseConfig rawConfig
  Logger.Display.withHandle cLogger $ \hLogger -> do
    offset <- newIORef 0
    let hRequest = Request.Handle offset cRequest hLogger
    updates <- runExceptT $ Request.getUpdates hRequest
    print updates
-- TODO withHandle должен реализовывать функционал такой, что должен закрываться и handle (файловый)
-- логгер реализован
-- закрывай отдельно
-- отдельно реализовать еще сервис HTTP, полностью независимо, читай статьи
-- теперь вот эти открытые хэндлы наверное можно закинуть в Reader
-- подумать как вынести открытие файла и IHandle в отдельный модуль
-- очевидно, forever должен быть внутри State, т.к. мапа с репитами для пользователей это стейт
