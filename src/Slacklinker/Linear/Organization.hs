{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Slacklinker.Linear.Organization (LinearOrganizationMetadata (..), linearOrganizationMetadata) where

import Data.GraphQL (get)
import Slacklinker.Linear.GraphQL (runLinearGraphQL, runQueryThrow)
import Slacklinker.Linear.GraphQL.API (WorkspaceInfoQuery (..))
import Slacklinker.Linear.Types (LinearBearerToken)
import Slacklinker.Prelude

data LinearOrganizationMetadata = LinearOrganizationMetadata
  { id :: Text
  -- ^ UUID for the org
  , urlKey :: Text
  -- ^ e.g. @mercury@ in https://linear.app/mercury
  , name :: Text
  -- ^ Display name for the org
  }
  deriving stock (Show)

-- | Retrieve organization-wide metadata from Linear
linearOrganizationMetadata :: (MonadIO m) => LinearBearerToken -> m LinearOrganizationMetadata
linearOrganizationMetadata token =
  toLinearOrganizationMetadata <$> runLinearGraphQL token (runQueryThrow WorkspaceInfoQuery)
  where
    toLinearOrganizationMetadata r =
      LinearOrganizationMetadata
        { id = [get| r.organization.id |]
        , urlKey = [get| r.organization.urlKey |]
        , name = [get| r.organization.name |]
        }
