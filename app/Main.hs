module Main where

import qualified Data.ByteString.Char8 as BC
import           Network.HTTP.Simple

token :: BC.ByteString
token = "1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU"

getUpdatesRequest :: Request
getUpdatesRequest =
  setRequestMethod "GET" $
  setRequestSecure True $
  "https://api.telegram.org/bot1913597879:AAEQ8hYhCyNoavFzxHWYcf2Lg-ejOSt48NU/getUpdates"

main :: IO ()
main = do
  response <- httpBS $ getUpdatesRequest
  let code = getResponseStatusCode response
  putStrLn $ mconcat ["Response code is ", show code]
  if code == 200
    then do
      let body = getResponseBody response
      putStrLn "writing to file..."
      BC.writeFile "updates.json" body
    else putStrLn "request failed with error"
