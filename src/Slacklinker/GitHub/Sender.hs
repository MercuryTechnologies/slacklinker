-- | Sender jobs for GitHub PR tracking: fetching PR state and syncing reactions.
module Slacklinker.GitHub.Sender (doTrackGithubPr, doRefreshGithubPr) where

import Database.Persist qualified as P
import Slacklinker.App (App (..), AppConfig (..), HasApp (..), runDB)
import Slacklinker.Exceptions (GithubDisabled (..))
import Slacklinker.GitHub.API (fetchPrState)
import Slacklinker.GitHub.DB (linksForPullRequest)
import Slacklinker.GitHub.Types
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender.Internal (runSlackEither)
import Slacklinker.Sender.Types (WorkspaceMeta (..), workspaceMetaFromWorkspaceE)
import Slacklinker.Slack.Reactions qualified as Reactions
import Slacklinker.Types (SlackToken)
import Web.Slack.Common (ResponseSlackError (..), SlackClientError (..))
import Web.Slack.Conversation (ConversationId (..))
import Web.Slack.Reactions qualified as Reactions
import Web.Slack.Types (Emoji (..))

getGithubToken :: (HasApp m, MonadIO m) => m GithubToken
getGithubToken = getsApp (.config.githubCreds) >>= (`orThrow` GithubDisabled) <&> (.token)

-- | Only the sender thread inserts these, so check-then-insert is fine.
getOrInsertPullRequest :: (MonadIO m) => GithubRepoId -> Int -> SqlPersistT m GithubPullRequestId
getOrInsertPullRequest repoId number =
  either entityKey identity <$> P.insertBy GithubPullRequest {repoId, number}

-- | Fetches the PR, records the message linking it, and reacts with its state.
doTrackGithubPr :: (HasApp m, MonadIO m) => JoinedChannelId -> Text -> PrRef -> m ()
doTrackGithubPr joinedChannelId messageTs ref = do
  token <- getGithubToken
  mRepo <- runDB . P.getBy $ UniqueGithubRepo ref.repo.owner ref.repo.name
  for_ mRepo \(Entity repoId _) ->
    fetchPrState token ref >>= \case
      Left err -> logWarn $ "Failed to fetch " <> tshow ref <> ": " <> err
      Right st -> do
        prId <- runDB do
          prId <- getOrInsertPullRequest repoId ref.number
          void
            $ P.insertBy
              LinkedGithubPullRequest
                { pullRequestId = prId
                , joinedChannelId
                , messageTs
                , appliedStatus = Nothing
                , appliedReview = ReviewNone
                }
          pure prId
        syncAllLinks prId st

-- | Refetches a tracked PR (after a webhook) and updates reactions on every link.
doRefreshGithubPr :: (HasApp m, MonadIO m) => GithubPullRequestId -> m ()
doRefreshGithubPr prId = do
  token <- getGithubToken
  mRow <- runDB do
    mPr <- P.get prId
    for mPr \pr -> (\repo -> (pr, repo)) <$> P.getJust pr.repoId
  for_ mRow \(pr, repo) -> do
    let ref = PrRef {repo = RepoName {owner = repo.owner, name = repo.name}, number = pr.number}
    fetchPrState token ref >>= \case
      Left err -> logWarn $ "Failed to fetch " <> tshow ref <> ": " <> err
      Right st -> syncAllLinks prId st

syncAllLinks :: (HasApp m, MonadIO m) => GithubPullRequestId -> PrState -> m ()
syncAllLinks prId st = do
  links <- runDB $ linksForPullRequest prId
  for_ links \(linked, Entity _ channel, workspace) ->
    syncLinkReactions (workspaceMetaFromWorkspaceE workspace).token channel.channelId st linked

data ReactionResult = ReactionOk | ReactionFailed | MessageGone
  deriving stock (Eq)

{- | Swaps the reactions on a linked message to match the PR state. Only
records what succeeded, so failures are retried on the next update. Drops the
link if the message or channel is gone.
-}
syncLinkReactions :: (HasApp m, MonadIO m) => SlackToken -> ConversationId -> PrState -> Entity LinkedGithubPullRequest -> m ()
syncLinkReactions token channel st (Entity linkId linked) = do
  let wantStatus = Just st.status
      wantReview = shownReview st
  statusResult <- syncOne (linked.appliedStatus == wantStatus) (prStatusEmoji <$> linked.appliedStatus) (prStatusEmoji <$> wantStatus)
  reviewResult <- syncOne (linked.appliedReview == wantReview) (prReviewEmoji linked.appliedReview) (prReviewEmoji wantReview)
  let updates =
        [LinkedGithubPullRequestAppliedStatus P.=. wantStatus | statusResult == Just ReactionOk]
          <> [LinkedGithubPullRequestAppliedReview P.=. wantReview | reviewResult == Just ReactionOk]
  if Just MessageGone `elem` [statusResult, reviewResult]
    then runDB $ P.delete linkId
    else unless (null updates) . runDB $ P.update linkId updates
  where
    syncOne alreadySynced old new
      | alreadySynced = pure Nothing
      | otherwise = Just <$> swapReaction old new

    -- Remove only after adding succeeds, so the message never loses its status.
    swapReaction old new = do
      added <- maybe (pure ReactionOk) addReaction new
      if added == ReactionOk
        then maybe (pure ReactionOk) removeReaction old
        else pure added

    addReaction emoji =
      checked "add" emoji =<< runSlackEither token \cfg ->
        void <$> Reactions.reactionsAdd cfg Reactions.AddReq {channel, timestamp = linked.messageTs, name = Emoji emoji}

    removeReaction emoji =
      checked "remove" emoji =<< runSlackEither token \cfg ->
        void <$> Reactions.reactionsRemove cfg Reactions.RemoveReq {removeReqChannel = channel, removeReqTimestamp = linked.messageTs, removeReqName = Emoji emoji}

    checked :: (MonadLogger m') => Text -> Text -> Either SlackClientError () -> m' ReactionResult
    checked _ _ (Right ()) = pure ReactionOk
    checked _ _ (Left (SlackError err))
      | err.errorText `elem` ["already_reacted", "no_reaction"] = pure ReactionOk
      | err.errorText `elem` ["message_not_found", "channel_not_found"] = pure MessageGone
    checked op emoji (Left err) = do
      logWarn $ "Failed to " <> op <> " :" <> emoji <> ": on " <> linked.messageTs <> " in " <> channel.unConversationId <> ": " <> tshow err
      pure ReactionFailed
