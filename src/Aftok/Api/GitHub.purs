module Aftok.Api.GitHub
  ( GitHubUsername
  , RepoLink
  , LinkRepoResponse
  , getGitHubUsername
  , linkGitHubUsername
  , unlinkGitHubUsername
  , getProjectRepoLinks
  , linkProjectRepo
  , unlinkProjectRepo
  ) where

import Prelude
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (decodeJson, (.:))
import Data.Argonaut.Decode.Error (JsonDecodeError)
import Data.Argonaut.Encode (encodeJson)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff)
import Affjax.ResponseFormat as RF
import Affjax.RequestBody as RB
import Affjax.StatusCode (StatusCode(..))
import Affjax (printError)
import Aftok.Api.Xsrf (getWithCredentials, putWithXsrf, deleteWithXsrf, postWithXsrf)
import Aftok.Api.Types (APIError(..))
import Aftok.Types (ProjectId, pidStr)

-- | GitHub username linked to the current user
type GitHubUsername = { username :: Maybe String }

-- | A linked repository for a project
type RepoLink =
  { linkId :: String
  , owner :: String
  , repo :: String
  , createdAt :: String
  }

-- | Response when linking a repo (includes webhook secret)
type LinkRepoResponse =
  { linkId :: String
  , webhookSecret :: String
  }

-- | Decode a GitHubUsername from JSON
decodeGitHubUsername :: Json -> Either JsonDecodeError GitHubUsername
decodeGitHubUsername json = do
  obj <- decodeJson json
  username <- obj .: "username"
  pure { username }

-- | Decode a RepoLink from JSON
decodeRepoLink :: Json -> Either JsonDecodeError RepoLink
decodeRepoLink json = do
  obj <- decodeJson json
  linkId <- obj .: "linkId"
  owner <- obj .: "owner"
  repo <- obj .: "repo"
  createdAt <- obj .: "createdAt"
  pure { linkId, owner, repo, createdAt }

-- | Decode an array of RepoLinks
decodeRepoLinks :: Json -> Either JsonDecodeError (Array RepoLink)
decodeRepoLinks json = do
  arr <- decodeJson json
  traverse decodeRepoLink arr

-- | Decode a LinkRepoResponse from JSON
decodeLinkRepoResponse :: Json -> Either JsonDecodeError LinkRepoResponse
decodeLinkRepoResponse json = do
  obj <- decodeJson json
  linkId <- obj .: "linkId"
  webhookSecret <- obj .: "webhookSecret"
  pure { linkId, webhookSecret }

-- | Get the GitHub username linked to the current user
getGitHubUsername :: Aff (Either APIError GitHubUsername)
getGitHubUsername = do
  response <- getWithCredentials RF.json "/api/user/github"
  pure $ case response of
    Left err -> Left $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> Left Forbidden
      StatusCode 200 -> case decodeGitHubUsername r.body of
        Left e -> Left $ ParseFailure e
        Right result -> Right result
      other -> Left $ Error { status: Just other, message: r.statusText }

-- | Link a GitHub username to the current user
linkGitHubUsername :: String -> Aff (Either APIError Unit)
linkGitHubUsername username = do
  let body = RB.json $ encodeJson { username }
  response <- putWithXsrf RF.ignore "/api/user/github" (Just body)
  pure $ case response of
    Left err -> Left $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> Left Forbidden
      StatusCode 204 -> Right unit
      StatusCode 200 -> Right unit
      other -> Left $ Error { status: Just other, message: r.statusText }

-- | Unlink the GitHub username from the current user
unlinkGitHubUsername :: Aff (Either APIError Unit)
unlinkGitHubUsername = do
  response <- deleteWithXsrf RF.ignore "/api/user/github"
  pure $ case response of
    Left err -> Left $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> Left Forbidden
      StatusCode 204 -> Right unit
      StatusCode 200 -> Right unit
      other -> Left $ Error { status: Just other, message: r.statusText }

-- | Get all GitHub repos linked to a project
getProjectRepoLinks :: ProjectId -> Aff (Either APIError (Array RepoLink))
getProjectRepoLinks pid = do
  response <- getWithCredentials RF.json ("/api/projects/" <> pidStr pid <> "/github/repos")
  pure $ case response of
    Left err -> Left $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> Left Forbidden
      StatusCode 200 -> case decodeRepoLinks r.body of
        Left e -> Left $ ParseFailure e
        Right result -> Right result
      other -> Left $ Error { status: Just other, message: r.statusText }

-- | Link a GitHub repo to a project (returns webhook secret)
linkProjectRepo :: ProjectId -> String -> String -> Aff (Either APIError LinkRepoResponse)
linkProjectRepo pid owner repo = do
  let body = RB.json $ encodeJson { owner, repo }
  response <- postWithXsrf RF.json ("/api/projects/" <> pidStr pid <> "/github/repos") (Just body)
  pure $ case response of
    Left err -> Left $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> Left Forbidden
      StatusCode 200 -> case decodeLinkRepoResponse r.body of
        Left e -> Left $ ParseFailure e
        Right result -> Right result
      StatusCode 201 -> case decodeLinkRepoResponse r.body of
        Left e -> Left $ ParseFailure e
        Right result -> Right result
      other -> Left $ Error { status: Just other, message: r.statusText }

-- | Unlink a GitHub repo from a project
unlinkProjectRepo :: ProjectId -> String -> Aff (Either APIError Unit)
unlinkProjectRepo pid linkId = do
  response <- deleteWithXsrf RF.ignore ("/api/projects/" <> pidStr pid <> "/github/repos/" <> linkId)
  pure $ case response of
    Left err -> Left $ Error { status: Nothing, message: printError err }
    Right r -> case r.status of
      StatusCode 403 -> Left Forbidden
      StatusCode 204 -> Right unit
      StatusCode 200 -> Right unit
      other -> Left $ Error { status: Just other, message: r.statusText }
