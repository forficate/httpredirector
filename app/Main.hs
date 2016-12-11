module Main where

import ClassyPrelude

import Data.Either.Combinators
import Data.Text (split, strip)
import Data.Text.Encoding (decodeUtf8')
import Network.HTTP.Types
import Network.Wai hiding (queryString)
import Network.Wai.Handler.Warp
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

data Protocol = HTTP | HTTPS

instance Show Protocol where
  show HTTP = "http"
  show HTTPS = "https"

type HostValidator = Text -> Bool

main :: IO ()
main = do
  forceSSL   <- (== "TRUE") . toUpper . fromMaybe "" <$> lookupEnv "FORCE_SSL"
  forceWWW   <- (== "TRUE") . toUpper . fromMaybe "" <$> lookupEnv "FORCE_WWW"
  validHosts <- filter (not . null) . fmap strip . split (== ',') . pack . fromMaybe "" <$> lookupEnv "VALID_HOSTS"
  port       <- fromMaybe 8080 . (=<<) readMay <$> lookupEnv "PORT"
  if not forceSSL && not forceWWW then
    putStrLn "ERROR - No configuration options set. Neither FORCE_SSL or FORCE_WWW was set" >> exitFailure
  else
    run port $ app (if forceSSL then HTTPS else HTTP) forceWWW (if null validHosts then const True else hostnameValidator validHosts)

app :: Protocol -> Bool -> HostValidator -> Application
app protocol forceWWW validHost req =
  merge $ do
    host <- maybe (Left badRequest) Right (safeGetHost req) >>= ensure validHost forbidden -- Format ${HOST}:${PORT}
    path <- maybe (Left badRequest) Right (safeGetRequestPathInfo req)
    queryString <- maybe (Left badRequest) Right (safeGetQueryString req)
    let newHost = if forceWWW  && not ("www." `isPrefixOf` host) then "www." <> host else host
    Right (redirect $ tshow protocol <> "://" <> newHost <> path <> queryString)


hostnameValidator :: [Text] -> Text -> Bool
hostnameValidator validHosts host =
  sanitizedHost `elem` validHosts -- Host will include port so we ignore it
  where
    sanitizedHost = takeWhile (/= ':') host

merge :: Either a a -> a
merge =
  either id id

ensure :: (b -> Bool) -> a -> b -> Either a b
ensure predicate a b =
  if predicate b then Right b else Left a

redirect :: Text -> (Response -> IO ResponseReceived) -> IO ResponseReceived
redirect uri respond =
  respond $ responseLBS temporaryRedirect307 [(hLocation, encodeUtf8 uri)] ""

badRequest :: (Response -> IO ResponseReceived) -> IO ResponseReceived
badRequest respond =
  respond $ responseLBS badRequest400 [(hContentType, "text/plain")] "ERROR - Bad request"

forbidden :: (Response -> IO ResponseReceived) -> IO ResponseReceived
forbidden respond =
  respond $ responseLBS forbidden403 [(hContentType, "text/plain")] "ERROR - Forbidden"

safeGetHost :: Request -> Maybe Text
safeGetHost =
  (=<<) (rightToMaybe . decodeUtf8') . requestHeaderHost

safeGetRequestPathInfo :: Request -> Maybe Text
safeGetRequestPathInfo =
  rightToMaybe . decodeUtf8' . rawPathInfo

safeGetQueryString :: Request -> Maybe Text
safeGetQueryString =
  rightToMaybe . decodeUtf8' . rawQueryString
