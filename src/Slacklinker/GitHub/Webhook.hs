-- | Slack message handling for GitHub PR tracking.
module Slacklinker.GitHub.Webhook where

import Data.Set qualified as Set
import Slacklinker.App (App (..), AppConfig (..), HasApp (..), runDB)
import Slacklinker.Extract.Types (ExtractedMessageData (..))
import Slacklinker.GitHub.DB (knownGithubRepos)
import Slacklinker.GitHub.Extract (extractPrRefs)
import Slacklinker.GitHub.Types (PrRef (..))
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender.Types (SenderRequest (..), senderEnqueue, workspaceMetaFromWorkspaceE)

{- | The message's only PR from a repo with webhooks. Messages linking several
are skipped: they'd share one set of reactions.
-}
singleTrackableRef :: Set (Text, Text) -> [PrRef] -> Maybe PrRef
singleTrackableRef known refs = case filter (\r -> (r.owner, r.repo) `Set.member` known) refs of
  [ref] -> Just ref
  _ -> Nothing

{- | Queues tracking for a PR link in a message, if the channel opted in and the
repo has webhooks set up.
-}
handleGithubChannelMessage :: (HasApp m, MonadIO m) => Entity Workspace -> Entity JoinedChannel -> ExtractedMessageData -> m ()
handleGithubChannelMessage workspaceE (Entity joinedChannelId joinedChannel) messageData = do
  configured <- isJust <$> getsApp (.config.githubCreds)
  let refs = extractPrRefs messageData.text
  when (configured && joinedChannel.trackGithubPrs && not (null refs)) do
    known <- runDB $ knownGithubRepos refs
    for_ (singleTrackableRef known refs)
      $ senderEnqueue
      . TrackGithubPr (workspaceMetaFromWorkspaceE workspaceE) joinedChannelId messageData.ts
