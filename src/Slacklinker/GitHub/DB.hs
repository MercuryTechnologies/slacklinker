module Slacklinker.GitHub.DB where

import Data.Set qualified as Set
import Database.Esqueleto.Experimental hiding ((<&>))
import Database.Persist qualified as P
import Slacklinker.Exceptions (SlacklinkerBug (..))
import Slacklinker.GitHub.Types (RepoName (..))
import Slacklinker.Models
import Slacklinker.Prelude

-- | Every Slack message linking the pull request, with its channel and workspace.
linksForPullRequest ::
  (MonadIO m) =>
  GithubPullRequestId ->
  SqlPersistT m [(Entity LinkedGithubPullRequest, Entity JoinedChannel, Entity Workspace)]
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

-- | The (owner, name) pairs among the given PRs' repos that have webhooks set up.
knownGithubRepos :: (MonadIO m) => [RepoName] -> SqlPersistT m (Set RepoName)
knownGithubRepos names = do
  repos <- select do
    repo <- from $ table @GithubRepo
    where_ $ repo.owner `in_` valList (ordNub $ (.owner) <$> names)
    pure (repo.owner, repo.name)
  pure . Set.fromList $ repos <&> \(Value owner, Value name) -> RepoName {owner, name}

-- | Records that a repo has webhooks. Safe against concurrent deliveries.
registerGithubRepo :: (MonadIO m) => RepoName -> SqlPersistT m GithubRepoId
registerGithubRepo RepoName {owner, name} = do
  rawExecute "insert into github_repos (owner, name) values (?, ?) on conflict do nothing" [P.toPersistValue owner, P.toPersistValue name]
  P.getBy (UniqueGithubRepo owner name) >>= \case
    Just repo -> pure $ entityKey repo
    Nothing -> throwIO $ SlacklinkerBug "github repo missing after insert"
