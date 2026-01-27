-- | XSRF token handling for API requests
-- |
-- | This module provides functions to read the XSRF cookie and make
-- | HTTP requests with the X-XSRF-TOKEN header attached.
module Aftok.Api.Xsrf
  ( getXsrfToken
  , getWithCredentials
  , postWithXsrf
  , putWithXsrf
  , deleteWithXsrf
  , patchWithXsrf
  ) where

import Prelude

import Affjax (Error, Response, defaultRequest, request)
import Affjax.RequestBody (RequestBody)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (ResponseFormat)
import Affjax.Web (driver)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toMaybe)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)

-- | The name of the XSRF cookie set by servant-auth-server
xsrfCookieName :: String
xsrfCookieName = "XSRF-TOKEN"

-- | The header name expected by servant-auth-server
xsrfHeaderName :: String
xsrfHeaderName = "X-XSRF-TOKEN"

-- | FFI to read a cookie value by name
foreign import getCookieImpl :: String -> Effect (Nullable String)

-- | Get the XSRF token from the cookie, if present
getXsrfToken :: Effect (Maybe String)
getXsrfToken = toMaybe <$> getCookieImpl xsrfCookieName

-- | Build the XSRF header if the token is present
xsrfHeader :: Maybe String -> Array RequestHeader
xsrfHeader = case _ of
  Nothing -> []
  Just token -> [RequestHeader xsrfHeaderName token]

-- | GET request with credentials (sends cookies)
getWithCredentials
  :: forall a
   . ResponseFormat a
  -> String
  -> Aff (Either Error (Response a))
getWithCredentials rf url = do
  request driver $ defaultRequest
    { method = Left GET
    , url = url
    , responseFormat = rf
    , withCredentials = true
    }

-- | POST request with XSRF token header
postWithXsrf
  :: forall a
   . ResponseFormat a
  -> String
  -> Maybe RequestBody
  -> Aff (Either Error (Response a))
postWithXsrf rf url content = do
  token <- liftEffect getXsrfToken
  request driver $ defaultRequest
    { method = Left POST
    , url = url
    , headers = xsrfHeader token
    , content = content
    , responseFormat = rf
    , withCredentials = true
    }

-- | PUT request with XSRF token header
putWithXsrf
  :: forall a
   . ResponseFormat a
  -> String
  -> Maybe RequestBody
  -> Aff (Either Error (Response a))
putWithXsrf rf url content = do
  token <- liftEffect getXsrfToken
  request driver $ defaultRequest
    { method = Left PUT
    , url = url
    , headers = xsrfHeader token
    , content = content
    , responseFormat = rf
    , withCredentials = true
    }

-- | DELETE request with XSRF token header
deleteWithXsrf
  :: forall a
   . ResponseFormat a
  -> String
  -> Aff (Either Error (Response a))
deleteWithXsrf rf url = do
  token <- liftEffect getXsrfToken
  request driver $ defaultRequest
    { method = Left DELETE
    , url = url
    , headers = xsrfHeader token
    , responseFormat = rf
    , withCredentials = true
    }

-- | PATCH request with XSRF token header
patchWithXsrf
  :: forall a
   . ResponseFormat a
  -> String
  -> RequestBody
  -> Aff (Either Error (Response a))
patchWithXsrf rf url content = do
  token <- liftEffect getXsrfToken
  request driver $ defaultRequest
    { method = Left PATCH
    , url = url
    , headers = xsrfHeader token
    , content = Just content
    , responseFormat = rf
    , withCredentials = true
    }
