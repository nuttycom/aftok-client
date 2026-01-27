module Aftok.Api.Account where

import Prelude
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class.Console (log)
import Affjax (get, printError)
import Affjax.StatusCode (StatusCode(..))
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Affjax.Web (driver)
import Aftok.Api.Xsrf (getWithCredentials, postWithXsrf)

type LoginRequest = { username :: String, password :: String }

data LoginResponse
  = LoginOK
  | LoginForbidden
  | LoginError { status :: Maybe StatusCode, message :: String }

-- | Post credentials to the login service and interpret the response
login :: String -> String -> Aff LoginResponse
login user pass = do
  log "Sending login request to /api/login ..."
  result <- postWithXsrf RF.ignore "/api/login" (Just <<< RB.Json <<< encodeJson $ { username: user, password: pass })
  case result of
    Left err -> log ("Login failed: " <> printError err)
    Right r -> log ("Login status: " <> show r.status)
  pure
    $ case result of
        Left err -> LoginError { status: Nothing, message: printError err }
        Right r -> case r.status of
          StatusCode 403 -> LoginForbidden
          StatusCode 200 -> LoginOK
          other -> LoginError { status: Just other, message: r.statusText }

checkLogin :: Aff LoginResponse
checkLogin = do
  result <- getWithCredentials RF.ignore "/api/login/check"
  case result of
    Left err -> do
      pure $ LoginError { status: Nothing, message: printError err }
    Right r -> do
      pure
        $ case r.status of
            StatusCode 200 -> LoginOK
            StatusCode _ -> LoginForbidden

logout :: Aff Unit
logout = void $ getWithCredentials RF.ignore "/api/logout"

data RecoverBy
  = RecoverByEmail String
  | RecoverByZAddr String

type SignupRequest =
  { username :: String
  , password :: String
  , recoverBy :: RecoverBy
  , captchaToken :: String
  , invitationCodes :: Array String
  }

signupRequest :: String -> String -> RecoverBy -> String -> Array String -> SignupRequest
signupRequest username password recoverBy captchaToken invitationCodes = { username, password, recoverBy, captchaToken, invitationCodes }

data SignupResponse
  = SignupOK
  | CaptchaInvalid
  | ZAddrInvalid
  | UsernameTaken
  | ServiceError (Maybe StatusCode) String

instance srShow :: Show SignupResponse where
  show r = case r of
    SignupOK -> "SignupOK"
    CaptchaInvalid -> "CaptchaInvalid"
    ZAddrInvalid -> "ZAddrInvalid"
    UsernameTaken -> "UsernameTaken"
    ServiceError _ _ -> "ServiceError"

data UsernameCheckResponse
  = UsernameCheckOK
  | UsernameCheckTaken

data ZAddrCheckResponse
  = ZAddrCheckValid
  | ZAddrCheckInvalid

checkUsername :: String -> Aff UsernameCheckResponse
checkUsername uname = do
  result <- get driver RF.json ("/api/check_username?username=" <> uname)
  pure
    $ case result of
        Left _ -> UsernameCheckTaken
        Right r
          | r.status == StatusCode 200 ->
              case decodeJson r.body of
                Right obj -> case obj .: "usernameAvailable" of
                  Right true -> UsernameCheckOK
                  _ -> UsernameCheckTaken
                Left _ -> UsernameCheckTaken
        Right _ -> UsernameCheckTaken

checkZAddr :: String -> Aff ZAddrCheckResponse
checkZAddr zaddr = do
  result <- get driver RF.json ("/api/validate_zaddr?zaddr=" <> zaddr)
  pure
    $ case result of
        Left _ -> ZAddrCheckInvalid
        Right r
          | r.status == StatusCode 200 ->
              case decodeJson r.body of
                Right obj -> case obj .: "zaddrValid" of
                  Right true -> ZAddrCheckValid
                  _ -> ZAddrCheckInvalid
                Left _ -> ZAddrCheckInvalid
        Right _ -> ZAddrCheckInvalid

signup :: SignupRequest -> Aff SignupResponse
signup req = do
  let
    signupJSON =
      encodeJson
        $
          { username: req.username
          , password: req.password
          , recoveryType:
              case req.recoverBy of
                RecoverByEmail _ -> "email"
                RecoverByZAddr _ -> "zaddr"
          , recoveryEmail:
              case req.recoverBy of
                RecoverByEmail email -> Just email
                RecoverByZAddr _ -> Nothing
          , recoveryZAddr:
              case req.recoverBy of
                RecoverByEmail _ -> Nothing
                RecoverByZAddr zaddr -> Just zaddr
          , captchaToken: req.captchaToken
          , invitation_codes: req.invitationCodes
          }
  log ("Sending JSON request: " <> stringify signupJSON)
  result <- postWithXsrf RF.ignore "/api/register" (Just <<< RB.Json $ signupJSON)
  case result of
    Left err -> do
      log ("Registration failed: " <> printError err)
      pure (ServiceError Nothing $ printError err)
    Right r
      | r.status == StatusCode 200 -> do
          log "Registration succeeded!"
          pure SignupOK
    Right r
      | r.status == StatusCode 403 -> do
          log ("Registration failed: Capcha Invalid")
          pure CaptchaInvalid
    Right r
      | r.status == StatusCode 400 -> do
          log ("Registration failed: Z-Address Invalid")
          pure ZAddrInvalid
    Right r -> do
      log ("Registration failed: " <> r.statusText)
      pure $ ServiceError (Just r.status) r.statusText

-- Password Reset Types and Functions

data PasswordResetRequestBody
  = ResetByUsername String
  | ResetByEmail String

data PasswordResetRequestResponse
  = ResetRequestSent
  | ResetRequestError { status :: Maybe StatusCode, message :: String }

data PasswordResetConfirmResponse
  = ResetConfirmOK
  | ResetConfirmInvalidToken
  | ResetConfirmError { status :: Maybe StatusCode, message :: String }

-- | Request a password reset link to be sent to the user's email
requestPasswordReset :: PasswordResetRequestBody -> Aff PasswordResetRequestResponse
requestPasswordReset req = do
  let
    body = encodeJson $ case req of
      ResetByUsername uname -> { username: Just uname, email: Nothing :: Maybe String }
      ResetByEmail email -> { username: Nothing :: Maybe String, email: Just email }
  log "Sending password reset request..."
  result <- postWithXsrf RF.ignore "/api/password-reset/request" (Just <<< RB.Json $ body)
  case result of
    Left err -> do
      log ("Password reset request failed: " <> printError err)
      pure $ ResetRequestError { status: Nothing, message: printError err }
    Right r
      | r.status == StatusCode 200 -> do
          log "Password reset request sent successfully"
          pure ResetRequestSent
    Right r -> do
      log ("Password reset request failed: " <> r.statusText)
      pure $ ResetRequestError { status: Just r.status, message: r.statusText }

-- | Confirm password reset with token and new password
confirmPasswordReset :: String -> String -> Aff PasswordResetConfirmResponse
confirmPasswordReset token newPassword = do
  let body = encodeJson { token, newPassword }
  log "Sending password reset confirmation..."
  result <- postWithXsrf RF.ignore "/api/password-reset/reset" (Just <<< RB.Json $ body)
  case result of
    Left err -> do
      log ("Password reset confirmation failed: " <> printError err)
      pure $ ResetConfirmError { status: Nothing, message: printError err }
    Right r
      -- Accept both 200 and 204 as success (server may return either)
      | r.status == StatusCode 200 || r.status == StatusCode 204 -> do
          log "Password reset successful!"
          pure ResetConfirmOK
      | r.status == StatusCode 400 -> do
          log "Password reset failed: Invalid or expired token"
          pure ResetConfirmInvalidToken
    Right r -> do
      log ("Password reset confirmation failed: " <> r.statusText)
      pure $ ResetConfirmError { status: Just r.status, message: r.statusText }
