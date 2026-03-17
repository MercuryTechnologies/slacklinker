-- | Types for the Slacklinker Linear integration
module Slacklinker.Linear.Types (
  LinearClientId (..),
  LinearClientSecret (..),
  LinearCreds (..),
  LinearBearerToken (..),
  LinearRefreshToken (..),
  LinearTicketUUID (..),
  LinearTicketId (..),
  linearTicketIdToText,
) where

import Database.Persist.Class (PersistField)
import Database.Persist.Sql (PersistFieldSql)
import Slacklinker.Prelude
import Slacklinker.UUID (UUID)

newtype LinearClientId = LinearClientId {unLinearClientId :: Text}
  deriving stock (Show)

newtype LinearClientSecret = LinearClientSecret {unLinearClientSecret :: Text}

instance Show LinearClientSecret where
  show _ = "LinearClientSecret REDACTED"

-- | Credentials owned by the app itself for access to Linear.
data LinearCreds = LinearCreds
  { linearClientId :: LinearClientId
  , linearClientSecret :: LinearClientSecret
  }
  deriving stock (Show)

-- | Bearer token for the Linear API. This belongs to one Slacklinker tenant.
newtype LinearBearerToken = LinearBearerToken {unLinearBearerToken :: Text}
  deriving newtype (Eq, PersistField, PersistFieldSql)

instance Show LinearBearerToken where
  show _ = "LinearBearerToken REDACTED"

-- | Bearer token for the Linear API. This belongs to one Slacklinker tenant.
newtype LinearRefreshToken = LinearRefreshToken {unLinearRefreshToken :: Text}
  deriving newtype (PersistField, PersistFieldSql)

instance Show LinearRefreshToken where
  show _ = "LinearRefreshToken REDACTED"

-- | Linear-side ticket UUID
newtype LinearTicketUUID = LinearTicketUUID {unLinearTicketId :: UUID}
  deriving newtype (PersistField, PersistFieldSql)
  deriving stock (Show)

-- | Linear ticket FOO-123
data LinearTicketId = LinearTicketId {team :: Text, number :: Int}
  deriving stock (Show, Eq, Ord)

linearTicketIdToText :: LinearTicketId -> Text
linearTicketIdToText (LinearTicketId team id) = team <> "-" <> tshow id
