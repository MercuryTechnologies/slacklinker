{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | GitHub webhook endpoint.
module Slacklinker.GitHub.Handler (
  Api,
  githubH,
  verifyGithubSignature,
  GithubWebhookPayload (..),
  GhRepo (..),
  GhOwner (..),
  GhPullRequestRef (..),
  payloadRepo,
  changesPrState,
) where

import Crypto.Hash (SHA256, digestFromByteString)
import Crypto.MAC.HMAC (HMAC (..), hmac)
import Data.Aeson (Value (..))
import Data.ByteString.Base16 qualified as B16
import Data.Either.Combinators (rightToMaybe)
import Data.Text qualified as T
import Database.Persist qualified as P
import Servant.API (Header', JSON, Post, ReqBody, Required, Strict, (:>))
import Slacklinker.App (App (..), AppConfig (..), AppM, HasApp (..), runDB)
import Slacklinker.Exceptions (GithubBadSignature (..), GithubDisabled (..))
import Slacklinker.GitHub.DB (registerGithubRepo)
import Slacklinker.GitHub.Types
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender.Types (SenderRequest (..), senderEnqueue)
import Slacklinker.Types (JSONByteString)

type RequiredHeader = Header' '[Required, Strict]

type Api =
  "webhook"
    :> RequiredHeader "X-GitHub-Event" Text
    :> RequiredHeader "X-Hub-Signature-256" Text
    :> ReqBody '[JSONByteString] ByteString
    :> Post '[JSON] Value

newtype GhOwner = GhOwner {login :: Text}

deriveFromJSON defaultOptions ''GhOwner

data GhRepo = GhRepo {name :: Text, owner :: GhOwner}

deriveFromJSON defaultOptions ''GhRepo

newtype GhPullRequestRef = GhPullRequestRef {number :: Int}

deriveFromJSON defaultOptions ''GhPullRequestRef

-- | The parts of a GitHub webhook payload we care about.
data GithubWebhookPayload = GithubWebhookPayload
  { action :: Maybe Text
  , repository :: Maybe GhRepo
  , pullRequest :: Maybe GhPullRequestRef
  }

deriveFromJSON snakeCaseOptions ''GithubWebhookPayload

-- | Lowercased (owner, name) of the payload's repo.
payloadRepo :: GithubWebhookPayload -> Maybe (Text, Text)
payloadRepo payload = payload.repository <&> \r -> (T.toLower r.owner.login, T.toLower r.name)

verifyGithubSignature :: GithubWebhookSecret -> Text -> ByteString -> Bool
verifyGithubSignature (GithubWebhookSecret secret) header body = fromMaybe False do
  hex <- T.stripPrefix "sha256=" header
  sig <- rightToMaybe . B16.decode $ encodeUtf8 hex
  digest <- digestFromByteString @SHA256 sig
  -- HMAC's Eq is constant time
  pure $ HMAC digest == hmac secret body

-- | Whether a webhook (event and action) can change a PR's status or review.
changesPrState :: Text -> Maybe Text -> Bool
changesPrState "pull_request" (Just action) =
  action `elem` ["opened", "reopened", "closed", "ready_for_review", "converted_to_draft", "enqueued", "dequeued"]
changesPrState "pull_request_review" (Just action) = action `elem` ["submitted", "dismissed"]
changesPrState _ _ = False

{- | Registers the repo as having webhooks (so its PR links get tracked), and
queues a refresh of the PR if it's one we track.
-}
postGithubWebhookR :: Text -> Text -> ByteString -> AppM Value
postGithubWebhookR event signature body = do
  creds <- getsApp (.config.githubCreds) >>= (`orThrow` GithubDisabled)
  unless (verifyGithubSignature creds.webhookSecret signature body) $ throwIO GithubBadSignature
  payload :: GithubWebhookPayload <- decodeThrow body
  for_ (payloadRepo payload) \(owner, name) -> do
    repoId <- runDB $ registerGithubRepo owner name
    when (changesPrState event payload.action) $ for_ payload.pullRequest \pr -> do
      mPr <- runDB . P.getBy $ UniqueGithubPullRequest repoId pr.number
      for_ mPr $ senderEnqueue . RefreshGithubPr . entityKey
  pure $ Object mempty

githubH :: Text -> Text -> ByteString -> AppM Value
githubH = postGithubWebhookR
