{-# LANGUAGE TemplateHaskell #-}

-- | Types for the GitHub pull request tracking integration.
module Slacklinker.GitHub.Types where

import Data.Text qualified as T
import Slacklinker.Import
import Slacklinker.Persist.DeriveEnum (derivePostgresEnumForSumType)

newtype GithubToken = GithubToken {unGithubToken :: Text}

instance Show GithubToken where
  show _ = "GithubToken REDACTED"

newtype GithubWebhookSecret = GithubWebhookSecret {unGithubWebhookSecret :: ByteString}

instance Show GithubWebhookSecret where
  show _ = "GithubWebhookSecret REDACTED"

data GithubCreds = GithubCreds
  { token :: GithubToken
  , webhookSecret :: GithubWebhookSecret
  }
  deriving stock (Show)

data RepoName = RepoName {owner :: Text, name :: Text}
  deriving stock (Show, Eq, Ord)

mkRepoName :: Text -> Text -> RepoName
mkRepoName owner name = RepoName {owner = T.toLower owner, name = T.toLower name}

data PrRef = PrRef {repo :: RepoName, number :: Int}
  deriving stock (Show, Eq, Ord)

data PrStatus = PrDraft | PrOpened | PrQueued | PrMerged | PrClosed
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

derivePostgresEnumForSumType ''PrStatus "github_pr_status"

data PrReview = ReviewNone | ReviewApproved | ReviewChangesRequested
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

derivePostgresEnumForSumType ''PrReview "github_pr_review"

data PrState = PrState {status :: PrStatus, review :: PrReview}
  deriving stock (Show, Eq)

prStatusEmoji :: PrStatus -> Text
prStatusEmoji = \case
  PrDraft -> "pr-draft"
  PrOpened -> "pr-opened"
  PrQueued -> "pr-queued"
  PrMerged -> "pr-merged"
  PrClosed -> "pr-closed"

shownReview :: PrState -> PrReview
shownReview st
  | st.status `elem` [PrMerged, PrClosed] = ReviewNone
  | otherwise = st.review

prReviewEmoji :: PrReview -> Maybe Text
prReviewEmoji = \case
  ReviewNone -> Nothing
  ReviewApproved -> Just "pr-approved"
  ReviewChangesRequested -> Just "pr-requested-changes"
