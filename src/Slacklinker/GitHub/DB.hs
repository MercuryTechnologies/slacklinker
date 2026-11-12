module Slacklinker.GitHub.DB where

import Database.Esqueleto.Experimental
import Database.Esqueleto.PostgreSQL (insertSelectWithConflict)
import Slacklinker.Exceptions (SlacklinkerBug (..))
import Slacklinker.GitHub.Types (RepoName (..))
import Slacklinker.Models
import Slacklinker.Prelude hiding ((<&>))

linksForPullRequest :: (MonadIO m) => GithubPullRequestId -> SqlPersistT m [(Entity LinkedGithubPullRequest, Entity JoinedChannel, Entity Workspace)]
linksForPullRequest prId = select do
  (linked :& channel :& workspace) <-
    from
      $ table @LinkedGithubPullRequest
      `innerJoin` table @JoinedChannel
        `on` (\(linked :& channel) -> linked.joinedChannelId ==. channel.id)
      `innerJoin` table @Workspace
        `on` (\(_ :& channel :& workspace) -> channel.workspaceId ==. workspace.id)
  where_ $ linked.pullRequestId ==. val prId
  pure (linked, channel, workspace)

-- | Records that a repo has webhooks. Safe against concurrent deliveries.
registerGithubRepo :: (MonadIO m) => RepoName -> SqlPersistT m GithubRepoId
registerGithubRepo RepoName {owner, name} = do
  insertSelectWithConflict UniqueGithubRepo (pure $ GithubRepo <# val owner <&> val name) (\_ _ -> [])
  getBy (UniqueGithubRepo owner name) >>= \case
    Just repo -> pure $ entityKey repo
    Nothing -> throwIO $ SlacklinkerBug "github repo missing after insert"
