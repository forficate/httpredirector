module Main where

import ClassyPrelude

import Data.Either.Combinators
import Data.Text.Encoding (decodeUtf8')
import Network.HTTP.Types
import Network.Wai hiding (queryString)
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

main :: IO ()
main = do
  forceSSL <- (== "TRUE") . toUpper . fromMaybe "" <$> lookupEnv "FORCE_SSL"
  forceWWW <- (== "TRUE") . toUpper . fromMaybe "" <$> lookupEnv "FORCE_WWW"
  port     <- fromMaybe 8080 . (=<<) readMay <$> lookupEnv "PORT"
  if not forceSSL && not forceWWW then
    putStrLn "ERROR - No configuration options set. Neither FORCE_SSL or FORCE_WWW was set" >> exitFailure
  else
    run port $ app (if forceSSL then "https" else "http") forceWWW


app :: Text -> Bool -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
app protocol forceWWW req = merge $ do
  host <- maybe (Left badRequest) Right (safeGetHost req)
  path <- maybe (Left badRequest) Right (safeGetRequestPathInfo req)
  queryString <- maybe (Left badRequest) Right (safeGetQueryString req)
  let newHost = if forceWWW  && not ("www" `isPrefixOf` host) then "www." <> host else host
  let redirectUrl = protocol <> "://" <> newHost <> path <> queryString
  Right (redirect redirectUrl)

merge :: Either a a -> a
merge = either id id

redirect :: Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
redirect uri respond = respond $ responseLBS temporaryRedirect307 [(hLocation, encodeUtf8 uri)] ""

badRequest :: (Response -> IO ResponseReceived) -> IO ResponseReceived
badRequest respond = respond $ responseLBS badRequest400 [] "ERROR - Bad request"


safeGetHost :: Request -> Maybe Text
safeGetHost = (=<<) (rightToMaybe . decodeUtf8') . requestHeaderHost

safeGetRequestPathInfo :: Request -> Maybe Text
safeGetRequestPathInfo = rightToMaybe . decodeUtf8' . rawPathInfo

safeGetQueryString :: Request -> Maybe Text
safeGetQueryString = rightToMaybe . decodeUtf8' . rawQueryString
