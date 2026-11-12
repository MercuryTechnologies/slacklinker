-- | Slack message handling for GitHub PR tracking.
module Slacklinker.GitHub.Webhook where

import Database.Persist (existsBy)
import Slacklinker.App (App (..), AppConfig (..), HasApp (..), runDB)
import Slacklinker.Extract.Types (ExtractedMessageData (..))
import Slacklinker.GitHub.Extract (extractPrRefs)
import Slacklinker.GitHub.Types (PrRef (..), RepoName (..))
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender.Types (SenderRequest (..), senderEnqueue, workspaceMetaFromWorkspaceE)

handleGithubChannelMessage :: (HasApp m, MonadIO m) => Entity Workspace -> Entity JoinedChannel -> ExtractedMessageData -> m ()
handleGithubChannelMessage workspaceE (Entity joinedChannelId joinedChannel) messageData = do
  configured <- isJust <$> getsApp (.config.githubCreds)
  when (configured && joinedChannel.trackGithubPrs) do
    case extractPrRefs messageData.text of
      [ref] -> do
        known <- runDB . existsBy $ UniqueGithubRepo ref.repo.owner ref.repo.name
        when known . senderEnqueue $ TrackGithubPr (workspaceMetaFromWorkspaceE workspaceE) joinedChannelId messageData.ts ref
      _ -> pure ()
