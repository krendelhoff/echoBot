module Main where

import           Control.Monad         (forever)
import           Data.Aeson
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable         (traverse_)
import           Data.IORef
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           GHC.Generics
import           Network.HTTP.Simple

data Updates =
  Updates
    { ok     :: Bool
    , result :: [Update]
    }
  deriving (Show, Generic)

data Update =
  Update
    { update_id :: Int
    , message   :: Message
    }
  deriving (Show, Generic)

instance FromJSON Updates

instance FromJSON Message

instance FromJSON Chat

instance FromJSON Update

data Message =
  Message
    { chat :: Chat
    , text :: T.Text
    }
  deriving (Show, Generic)

data Chat =
  Chat
    { id       :: Int
    , username :: T.Text
    }
  deriving (Show, Generic)

getUpdatesRequest :: BC.ByteString -> Request
getUpdatesRequest offset =
  setRequestQueryString [("timeout", Just "10"), ("offset", Just offset)] $
  setRequestMethod "GET" $
  setRequestSecure True $
  "https://api.telegram.org/bot1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU/getUpdates"

main :: IO ()
main = do
  lastOffset <- newIORef 0
  forever $ do
    value <- readIORef lastOffset
    response <- httpBS $ getUpdatesRequest (BC.pack . show $ value)
    let code = getResponseStatusCode response
    putStrLn $ mconcat ["Response code is ", show code]
    if code == 200
      then do
        let body = getResponseBody response
        putStrLn "writing to file..."
        BC.writeFile "updates.json" body
        putStrLn "What was written?"
        let updates = decodeStrict body :: Maybe Updates
        maybe
          (putStrLn "Parse Error")
          (\upDates -> do
             let updateList = result upDates
             traverse_
               (\x -> do
                  writeIORef lastOffset (update_id x + 1)
                  TIO.putStrLn $
                    mconcat
                      [ (username (chat (message x)))
                      , " wrote: "
                      , text (message x)
                      ])
               updateList)
          updates
        putStr "last offset: "
        readIORef lastOffset >>= print
      else putStrLn "request failed with error"
