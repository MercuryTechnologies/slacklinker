{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

-- | Manages updating the Linear teams in the database.
module Slacklinker.Linear.Teams (getLinearTeamsUncached, updateLinearTeamsCache, updateAllLinearTeamsCaches) where

import Data.GraphQL (get)
import Data.Vector qualified as V
import Database.Esqueleto.Experimental (Value (..))
import Database.Esqueleto.Experimental qualified as E
import Database.Persist qualified as P
import Slacklinker.App (HasApp, runDB)
import Slacklinker.Exceptions (LinearNotAuthenticated (..))
import Slacklinker.Import (orThrow)
import Slacklinker.Linear.DB (linearAuthSessionForWorkspace, linearAuthSessions)
import Slacklinker.Linear.GraphQL (PageInfo (..), paginateQuery, runLinearGraphQL, runQueryThrow)
import Slacklinker.Linear.GraphQL.API (ListTeamsQuery (..))
import Slacklinker.Models
import Slacklinker.Prelude

data LinearTeamAPI = LinearTeamAPI
  { workspaceUrlKey :: Text
  , urlKey :: Text
  }
  deriving stock (Show)

getLinearTeamsUncached :: (HasApp m, MonadIO m) => WorkspaceId -> m (LinearOrganizationId, Vector LinearTeamAPI)
getLinearTeamsUncached workspaceId = do
  session_ <- runDB $ linearAuthSessionForWorkspace workspaceId
  (Value linearOrgId, Value token) <- session_ `orThrow` LinearNotAuthenticated

  (linearOrgId,) <$> run token
  where
    extractPageInfo obj = PageInfo {endCursor = [get| obj.endCursor |], hasNextPage = [get| obj.hasNextPage |]}

    run token = runLinearGraphQL token $ paginateQuery \startCursor -> do
      res <- runQueryThrow (ListTeamsQuery {_startCursor = startCursor})

      let
        workspaceUrlKey = [get| res.organization.urlKey |]
        thisResult =
          V.fromList
            . map
              (LinearTeamAPI workspaceUrlKey . [get| .key |])
            $ [get| res.teams.nodes |]
      pure (extractPageInfo [get| res.teams.pageInfo |], thisResult)

updateLinearTeamsCache :: (HasApp m, MonadIO m) => WorkspaceId -> m ()
updateLinearTeamsCache workspaceId = do
  (linearOrgId, teams) <- getLinearTeamsUncached workspaceId
  -- FIXME(jadel): delete obsolete linear teams
  runDB do
    for_ teams \team -> P.insertBy LinearTeam {linearOrganizationId = linearOrgId, urlKey = team.urlKey}

updateAllLinearTeamsCaches :: (HasApp m, MonadIO m) => m ()
updateAllLinearTeamsCaches = do
  orgs <- fmap (fmap unValue) . runDB $ E.select do
    (linearOrg, _session) <- linearAuthSessions
    pure linearOrg.workspaceId
  -- FIXME(jadel): handle exceptions if we start actually caring more about
  -- multitenancy
  for_ orgs updateLinearTeamsCache
