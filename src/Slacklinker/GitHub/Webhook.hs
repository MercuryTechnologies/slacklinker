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

{- | Queues tracking for a message's PR link, if it links exactly one PR, the
channel opted in and the repo has webhooks set up.
-}
handleGithubChannelMessage :: (HasApp m, MonadIO m) => Entity Workspace -> Entity JoinedChannel -> ExtractedMessageData -> m ()
handleGithubChannelMessage workspaceE (Entity joinedChannelId joinedChannel) messageData = do
  configured <- isJust <$> getsApp (.config.githubCreds)
  case extractPrRefs messageData.text of
    [ref] | configured && joinedChannel.trackGithubPrs -> do
      known <- runDB . existsBy $ UniqueGithubRepo ref.repo.owner ref.repo.name
      when known . senderEnqueue $ TrackGithubPr (workspaceMetaFromWorkspaceE workspaceE) joinedChannelId messageData.ts ref
    _ -> pure ()
