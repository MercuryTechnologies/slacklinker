module Slacklinker.Linear.DB (linearAuthSessions, linearAuthSessionForWorkspace, lockLinearAuthSession, knownLinearTeamUrlKeys) where

import Database.Esqueleto.Experimental
import Database.Esqueleto.PostgreSQL (forNoKeyUpdateOf)
import Database.Esqueleto.PostgreSQL qualified as DB
import Slacklinker.Models (
  LinearAPIAuthSession (..),
  LinearAPIAuthSessionId,
  LinearOrganization (..),
  LinearOrganizationId,
  LinearTeam (..),
  WorkspaceId,
 )
import Slacklinker.Prelude

linearAuthSessions :: SqlQuery (SqlExpr (Entity LinearOrganization), SqlExpr (Entity LinearAPIAuthSession))
linearAuthSessions = do
  (linearOrg :& session) <-
    from
      $ table @LinearOrganization
      `innerJoin` (table @LinearAPIAuthSession)
        `on` (\(linearOrg :& session) -> linearOrg.id ==. session.linearOrganizationId)
  pure (linearOrg, session)

-- | Gets a Linear token for the given workspace
linearAuthSessionForWorkspace ::
  (MonadIO m) =>
  WorkspaceId ->
  SqlPersistT m (Maybe (Value LinearOrganizationId, Entity LinearAPIAuthSession))
linearAuthSessionForWorkspace wsId = do
  selectOne do
    (linearOrg, session) <- linearAuthSessions
    where_ $ linearOrg.workspaceId ==. val wsId
    pure (linearOrg.id, session)

{- | Takes a lock on the Linear auth session to prevent concurrent updating of
auth sessions.
-}
lockLinearAuthSession :: (MonadIO m) => LinearAPIAuthSessionId -> SqlPersistT m ()
lockLinearAuthSession id = do
  void $ selectOne do
    it <- from $ table @LinearAPIAuthSession
    forNoKeyUpdateOf it DB.wait
    where_ $ it.id ==. val id
    pure ()

{- | Gets the subset of the given list of Linear team URL keys (e.g. DUX, FOO,
BAR, BAZ) which is known to Slacklinker to actually exist on Linear's side
(e.g. DUX).

This list is kept up to date by @UpdateLinearTeams@.

This avoids sending things to Linear which look like a ticket ID but might
be either sensitive or just pointless.
-}
knownLinearTeamUrlKeys :: (MonadIO m) => WorkspaceId -> [Text] -> SqlPersistT m [Text]
knownLinearTeamUrlKeys wsId teamsToCheck =
  fmap unValue <$> select do
    (linearTeam :& linearOrganization) <-
      from
        $ table @LinearTeam
        `innerJoin` (table @LinearOrganization)
          `on` (\(linearTeam :& linearOrg) -> linearTeam.linearOrganizationId ==. linearOrg.id)
    where_ $ linearOrganization.workspaceId ==. val wsId &&. linearTeam.urlKey `in_` valList teamsToCheck
    pure linearTeam.urlKey
