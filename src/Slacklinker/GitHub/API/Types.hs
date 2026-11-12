{-# LANGUAGE TemplateHaskell #-}

-- | GitHub GraphQL response types for the PR query in "Slacklinker.GitHub.API".
module Slacklinker.GitHub.API.Types where

import Data.Aeson (Value)
import Slacklinker.Import

data GhPrState = GhOpen | GhClosed | GhMerged

deriveFromJSON (dropConstructorPrefix "Gh" screamingSnakeCaseOptions) ''GhPrState

data GhReviewDecision = DecisionApproved | DecisionChangesRequested | DecisionReviewRequired

deriveFromJSON (dropConstructorPrefix "Decision" screamingSnakeCaseOptions) ''GhReviewDecision

data GhReviewState = StatePending | StateCommented | StateApproved | StateChangesRequested | StateDismissed
  deriving stock (Eq)

deriveFromJSON (dropConstructorPrefix "State" screamingSnakeCaseOptions) ''GhReviewState

newtype GhReview = GhReview {state :: GhReviewState}

deriveFromJSON defaultOptions ''GhReview

newtype GhReviews = GhReviews {nodes :: [GhReview]}

deriveFromJSON defaultOptions ''GhReviews

data GhPullRequest = GhPullRequest
  { state :: GhPrState
  , isDraft :: Bool
  , isInMergeQueue :: Bool
  , reviewDecision :: Maybe GhReviewDecision
  , latestOpinionatedReviews :: GhReviews
  }

deriveFromJSON defaultOptions ''GhPullRequest

newtype GhRepository = GhRepository {pullRequest :: Maybe GhPullRequest}

deriveFromJSON defaultOptions ''GhRepository

newtype GhData = GhData {repository :: Maybe GhRepository}

deriveFromJSON defaultOptions ''GhData

data GhResponse = GhResponse {responseData :: Maybe GhData, errors :: Maybe Value}

deriveFromJSON (renameField "responseData" "data" defaultOptions) ''GhResponse

-- | Body of a non-200 response.
newtype GhError = GhError {message :: Text}

deriveFromJSON defaultOptions ''GhError
