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
import Slacklinker.Linear.DB (linearAuthSessions)
import Slacklinker.Linear.GraphQL (PageInfo (..), paginateQuery, runLinearGraphQL, runQueryThrow)
import Slacklinker.Linear.GraphQL.API (ListTeamsQuery (..))
import Slacklinker.Linear.Session (getToken)
import Slacklinker.Models
import Slacklinker.Prelude

data LinearTeamAPI = LinearTeamAPI
  { workspaceUrlKey :: Text
  , urlKey :: Text
  }
  deriving stock (Show)

getLinearTeamsUncached :: (MonadUnliftIO m, HasApp m) => WorkspaceId -> m (LinearOrganizationId, Vector LinearTeamAPI)
getLinearTeamsUncached workspaceId = do
  (linearOrgId, token) <- getToken workspaceId

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

updateLinearTeamsCache :: (MonadUnliftIO m, HasApp m) => WorkspaceId -> m ()
updateLinearTeamsCache workspaceId = do
  (linearOrgId, teams) <- getLinearTeamsUncached workspaceId
  -- FIXME(jadel): delete obsolete linear teams
  runDB do
    for_ teams \team -> P.insertBy LinearTeam {linearOrganizationId = linearOrgId, urlKey = team.urlKey}

updateAllLinearTeamsCaches :: (MonadUnliftIO m, HasApp m) => m ()
updateAllLinearTeamsCaches = do
  orgs <- fmap (fmap unValue) . runDB $ E.select do
    (linearOrg, _session) <- linearAuthSessions
    pure linearOrg.workspaceId
  -- FIXME(jadel): handle exceptions if we start actually caring more about
  -- multitenancy
  for_ orgs updateLinearTeamsCache
