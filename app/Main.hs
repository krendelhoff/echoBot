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
  rawData <- BC.readFile "config.yaml"
  cLogger <- Logger.Display.parseConfig rawData
  cRequest <- Request.parseConfig rawData
  Logger.Display.withHandle cLogger $ \hLogger -> do
    let req = "https://www.google.com/"
    Request.withHandle cRequest hLogger req Nothing $ \hRequest -> do
      result <- runExceptT $ Request.perform hRequest
      either print print result
-- TODO withHandle должен реализовывать функционал такой, что должен закрываться и handle (файловый)
-- логгер реализован
-- закрывай отдельно
-- отдельно реализовать еще сервис HTTP, полностью независимо, читай статьи
-- теперь вот эти открытые хэндлы наверное можно закинуть в Reader
-- подумать как вынести открытие файла и IHandle в отдельный модуль
-- очевидно, forever должен быть внутри State, т.к. мапа с репитами для пользователей это стейт
-- не обяз везде withHandle - хэндл может жить и должно
-- мб таки создать requestInfo и создавать сам Request уже в perform
-- хотя это такое
