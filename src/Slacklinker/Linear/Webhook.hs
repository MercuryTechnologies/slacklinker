module Slacklinker.Linear.Webhook (shouldIgnore, handleLinearChannelMessage) where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
import OpenTelemetry.Trace.Core (Span)
import Slacklinker.App (HasApp, runDB)
import Slacklinker.Extract.Types (ExtractedMessageData (..), extractableMessageToSlackUrlParts)
import Slacklinker.Import
import Slacklinker.Linear.DB (knownLinearTeamUrlKeys)
import Slacklinker.Linear.Extract
import Slacklinker.Models
import Slacklinker.Sender (SenderRequest (..), senderEnqueue)
import Slacklinker.Sender.Types (workspaceMetaFromWorkspaceE)

{- | Ignores the bot messages from the Linear \"Linkback\" feature, since we
will catch the human/bot message that initiated them instead.
-}
shouldIgnore :: ExtractedMessageData -> Bool
shouldIgnore = isJust . ignoreReason

data IgnoreReason = LinearBot | LinearApp

ignoreReason :: ExtractedMessageData -> Maybe IgnoreReason
ignoreReason ExtractedMessageData {botDisplayName = Just dn}
  | "Linear" `isPrefixOf` dn = Just LinearBot
  -- Preserve the existing precedence: a non-Linear display name bypasses
  -- the app-ID exclusion, even if its app ID is Linear's.
  | otherwise = Nothing
-- HACK: We can't actually cleanly get info that this is Linear from the
-- webhook data we are sent. Hardcoding the Linear app ID feels deeply
-- unsatisfying.
--
-- This case deals with the "Create Issue" button in the context menu.
ignoreReason ExtractedMessageData {appId = Just "AEMQ3Q4F4"} = Just LinearApp
ignoreReason _ = Nothing

-- | Inserts todo items to backlink tickets in a message.
handleLinearChannelMessage ::
  (HasApp m, MonadIO m) =>
  Maybe Span ->
  Entity Workspace ->
  KnownUserId ->
  JoinedChannelId ->
  ExtractedMessageData ->
  m Int
handleLinearChannelMessage owner _ _ _ messageData | Just reason <- ignoreReason messageData = do
  for_ owner $ \span -> addAttribute span "linear.tickets.skip_reason" $ case reason of
    LinearBot -> "linear_bot" :: Text
    LinearApp -> "linear_app"
  pure 0
handleLinearChannelMessage
  owner
  workspaceE@(Entity workspaceId workspace)
  knownUserId
  joinedChannelId
  messageData = do
    let candidates = extractTicketIds messageData.text
        ticketIds = Set.fromList candidates
        uniqueTeams = Set.map (.team) ticketIds

    -- Candidate occurrences are before deduplication; unique and accepted
    -- counts are after deduplication by LinearTicketId (team and number).
    count "linear.tickets.candidate_count" $ length candidates
    count "linear.tickets.unique_candidate_count" $ Set.size ticketIds
    extantTeams <- Set.fromList <$> runDB (knownLinearTeamUrlKeys workspaceId (Set.toList uniqueTeams))

    let plausibleTicketIds = Set.filter ((`Set.member` extantTeams) . (.team)) ticketIds
        workspaceMeta = workspaceMetaFromWorkspaceE workspaceE
        slackUrlParts = extractableMessageToSlackUrlParts workspace.slackSubdomain messageData

    let plausibleTicketIds' = Set.toList plausibleTicketIds
    count "linear.tickets.known_team_count" $ Set.size plausibleTicketIds
    count "linear.tickets.jobs_queued" 0
    -- The entire accepted set is one job, regardless of ticket count.
    case NE.nonEmpty plausibleTicketIds' of
      Nothing -> pure 0
      Just tickets -> do
        senderEnqueue $ BacklinkPlausibleLinearTickets workspaceMeta slackUrlParts joinedChannelId knownUserId tickets
        count "linear.tickets.jobs_queued" 1
        pure 1
    where
      count name value = for_ owner $ \span -> addAttribute span name (value :: Int)
