module Main where

import           Data.Aeson
import qualified Data.ByteString.Lazy as L
import qualified Data.Text            as T
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

{-
token :: BC.ByteString
token = "1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU"
-}
baseRequest =
  "https://api.telegram.org/bot1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU/"

getUpdatesRequest :: Request
getUpdatesRequest =
  setRequestMethod "GET" $
  setRequestSecure True $
  "https://api.telegram.org/bot1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU/getUpdates"

--sendMessageRequest :: Int -> BC.ByteString -> Request
--sendMessageRequest chat_id text = set
main :: IO ()
main = do
  response <- httpLBS $ getUpdatesRequest
  let code = getResponseStatusCode response
  putStrLn $ mconcat ["Response code is ", show code]
  if code == 200
    then do
      let body = getResponseBody response
      putStrLn "writing to file..."
      L.writeFile "updates.json" body
      putStrLn "What was written?"
      let updates = decode body :: Maybe Updates
      maybe (putStrLn "Parse Error") print updates
    else putStrLn "request failed with error"
