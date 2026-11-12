module Slacklinker.GitHub.DB where

import Data.Set qualified as Set
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

knownGithubRepos :: (MonadIO m) => [RepoName] -> SqlPersistT m (Set RepoName)
knownGithubRepos [] = pure mempty
knownGithubRepos names = do
  repos <- select do
    repo <- from $ table @GithubRepo
    where_ . foldr (||.) (val False) $ (\n -> repo.owner ==. val n.owner &&. repo.name ==. val n.name) <$> names
    pure (repo.owner, repo.name)
  pure . Set.fromList $ (\(Value owner, Value name) -> RepoName {owner, name}) <$> repos

-- | Records that a repo has webhooks. Safe against concurrent deliveries.
registerGithubRepo :: (MonadIO m) => RepoName -> SqlPersistT m GithubRepoId
registerGithubRepo RepoName {owner, name} = do
  insertSelectWithConflict UniqueGithubRepo (pure $ GithubRepo <# val owner <&> val name) (\_ _ -> [])
  getBy (UniqueGithubRepo owner name) >>= \case
    Just repo -> pure $ entityKey repo
    Nothing -> throwIO $ SlacklinkerBug "github repo missing after insert"
