module Main where

import qualified Logger
import qualified Logger.Display
import qualified System.IO      as SIO

main :: IO ()
main = do
  let myConfig =
        Logger.Display.Config
          {mode = Logger.Stdout, priority = Logger.Info, file = Nothing}
      myIHandle =
        Logger.Display.IHandle {handle = SIO.stdout, config = myConfig}
  Logger.Display.withHandle myConfig myIHandle $ \handler -> do
    Logger.log handler "Biba - boba!"
-- TODO withHandle должен реализовывать функционал такой, что должен закрываться и handle (файловый)
-- отдельно реализовать еще сервис HTTP, полностью независимо, читай статьи
