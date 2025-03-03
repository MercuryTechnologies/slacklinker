{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Manages updating the Linear teams in the database.
module Slacklinker.Linear.Teams (updateLinearTeamsCache, updateAllLinearTeamsCaches) where

import Data.GraphQL (MonadGraphQLQuery (runQuerySafe), get)
import Database.Esqueleto.Experimental (Value (..))
import Database.Esqueleto.Experimental qualified as E
import Database.Persist qualified as P
import Slacklinker.App (HasApp, runDB)
import Slacklinker.Exceptions (LinearNotAuthenticated (..))
import Slacklinker.Import (orThrow)
import Slacklinker.Linear.DB (linearAuthSessionForWorkspace, linearAuthSessions)
import Slacklinker.Linear.GraphQL (resultThrow, runQuery)
import Slacklinker.Linear.GraphQL.API (ListTeamsQuery (..))
import Slacklinker.Models
import Slacklinker.Prelude

data LinearTeamAPI = LinearTeamAPI
  { workspaceUrlKey :: Text
  , urlKey :: Text
  }
  deriving stock (Show)

getLinearTeamsUncached :: (HasApp m, MonadIO m) => WorkspaceId -> m (LinearOrganizationId, [LinearTeamAPI])
getLinearTeamsUncached wsId = do
  -- FIXME(jadel): pagination
  sess_ <- runDB $ linearAuthSessionForWorkspace wsId
  (Value loId, Value token) <- sess_ `orThrow` LinearNotAuthenticated

  res <- resultThrow =<< runQuery token (runQuerySafe (ListTeamsQuery {_startCursor = Nothing}))

  let workspaceUrlKey = [get| res.organization.urlKey |]
  pure . (loId,) $ map (LinearTeamAPI workspaceUrlKey . [get| .key |]) [get| res.teams.nodes |]

updateLinearTeamsCache :: (HasApp m, MonadIO m) => WorkspaceId -> m ()
updateLinearTeamsCache wsId = do
  (loId, teams) <- getLinearTeamsUncached wsId
  -- FIXME(jadel): delete obsolete linear teams
  runDB do
    for_ teams \team -> P.insertBy LinearTeam {linearOrganizationId = loId, urlKey = team.urlKey}

updateAllLinearTeamsCaches :: (HasApp m, MonadIO m) => m ()
updateAllLinearTeamsCaches = do
  orgs <- fmap (fmap unValue) . runDB $ E.select do
    (lo, _sess) <- linearAuthSessions
    pure lo.workspaceId
  -- FIXME(jadel): handle exceptions if we start actually caring more about
  -- multitenancy
  for_ orgs updateLinearTeamsCache
