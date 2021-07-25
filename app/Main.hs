module Main where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Trans
import qualified Data.ByteString.Char8  as BC
import           Data.IORef
import           Data.Map               (Map)
import qualified Data.Map               as M

import           Telegram.Configuration
import           Telegram.ParseJSON

refreshOffset :: Updates -> Int -> Int
refreshOffset (Updates {result = []}) offsetValue = offsetValue
refreshOffset updates _ = succ . update_id . last . result $ updates

runBot config bot = runReaderT (runStateT (runExceptT bot) M.empty) config

-- короче, никакой runBot и вообще forever в main не сработает, т.к. постоянно надо обновлять состояние, а мы его ранботом теряем
-- делать IO state не вариант, надо чисто реализовывать
-- думой, читай пока про Tagless Final
-- надо уходить от IO и всё делать чистым
-- т.е. формировтаь запрос, узнавать сколько раз его отправить, всё это делать чисто
-- и только в самом конце кидать IO
bot ::
     ( MonadReader Config m
     , MonadState UserRepeat m
     , MonadIO m
     , MonadError String m
     )
  => IORef Int
  -> m ()
bot offset = do
  offst <- liftIO $ readIORef offset
  liftIO $ writeIORef offset (refreshOffset undefined offst)
  return ()

main :: IO ()
main = do
  lastOffset <- newIORef 0
  eitherConfig <- runExceptT parseConfig
  either
    putStrLn -- debug print-logging
    (\config -> do
       forever $ do
         res <- runBot config bot
         either putStrLn (const $ return ()) res -- debug print-logging
     )
    eitherConfig
