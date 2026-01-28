module Aftok.Api.Config where

import Prelude
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Affjax (get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.ResponseFormat as RF
import Affjax.Web (driver)

type ClientConfig =
  { recaptchaSiteKey :: String
  }

data ConfigResponse
  = ConfigOK ClientConfig
  | ConfigError String

-- | Fetch client configuration from the server
fetchConfig :: Aff ConfigResponse
fetchConfig = do
  log "Fetching client configuration from /api/config ..."
  result <- get driver RF.json "/api/config"
  case result of
    Left err -> do
      log ("Config fetch failed: " <> printError err)
      pure $ ConfigError (printError err)
    Right r
      | r.status == StatusCode 200 ->
          case decodeJson r.body of
            Right obj -> case obj .: "recaptchaSiteKey" of
              Right key -> do
                log "Config loaded successfully"
                pure $ ConfigOK { recaptchaSiteKey: key }
              Left decodeErr -> do
                log ("Failed to decode config: " <> show decodeErr)
                pure $ ConfigError "Failed to decode recaptchaSiteKey"
            Left decodeErr -> do
              log ("Failed to decode config response: " <> show decodeErr)
              pure $ ConfigError "Failed to decode config response"
    Right r -> do
      log ("Config fetch failed with status: " <> show r.status)
      pure $ ConfigError ("HTTP error: " <> r.statusText)
