module TestUtils where

import Database.Persist
import Slacklinker.App
import Slacklinker.Models
import Slacklinker.Types
import TestImport
import Web.Slack.Types (TeamId (..))

createWorkspace :: (HasApp m, MonadIO m) => m (WorkspaceId, TeamId)
createWorkspace = do
  let slackTeamId =  TeamId "T0123"
  wsId <- runDB $
    insert $
      Workspace
        { slackSubdomain = "jadeapptesting"
        -- FIXME(jadel): probably should randomize this
        , slackTeamId
        , slackOauthToken = SlackToken "abcde"
        }
  pure (wsId, slackTeamId)
