module Slacklinker.Linear.DB where

import Database.Esqueleto.Experimental
import Slacklinker.Linear.Types (LinearBearerToken)
import Slacklinker.Models (
  LinearAPIAuthSession (..),
  LinearOrganization (..),
  LinearOrganizationId,
  LinearTeam (..),
  WorkspaceId,
 )
import Slacklinker.Prelude

linearAuthSessions :: SqlQuery (SqlExpr (Entity LinearOrganization), SqlExpr (Entity LinearAPIAuthSession))
linearAuthSessions = do
  (lo :& sess) <-
    from
      $ table @LinearOrganization
      `innerJoin` (table @LinearAPIAuthSession)
        `on` (\(lo :& sess) -> lo.id ==. sess.linearOrganizationId)
  pure (lo, sess)

-- | Gets a linear token for the given workspace
linearAuthSessionForWorkspace ::
  (MonadIO m) =>
  WorkspaceId ->
  SqlPersistT m (Maybe (Value LinearOrganizationId, Value LinearBearerToken))
linearAuthSessionForWorkspace wsId = do
  selectOne do
    (lo, sess) <- linearAuthSessions
    where_ $ lo.workspaceId ==. val wsId
    pure (lo.id, sess.token)

-- | Gets the subset of teams which exist in the database
knownLinearTeamUrlKeys :: (MonadIO m) => WorkspaceId -> [Text] -> SqlPersistT m [Text]
knownLinearTeamUrlKeys wsId teamsToCheck =
  fmap unValue <$> select do
    (team :& linearOrganization) <-
      from
        $ table @LinearTeam
        `innerJoin` (table @LinearOrganization)
          `on` (\(t :& lo) -> t.linearOrganizationId ==. lo.id)
    where_ $ linearOrganization.workspaceId ==. val wsId &&. team.urlKey `in_` valList teamsToCheck
    pure team.urlKey
