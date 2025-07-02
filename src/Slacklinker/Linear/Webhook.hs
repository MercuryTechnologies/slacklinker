module Slacklinker.Linear.Webhook (shouldIgnore, handleLinearChannelMessage) where

import Data.List.NonEmpty qualified as NE
import Data.Set qualified as Set
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
shouldIgnore ExtractedMessageData {botDisplayName = Just dn} = "Linear" `isPrefixOf` dn
-- HACK: We can't actually cleanly get info that this is Linear from the
-- webhook data we are sent. Hardcoding the Linear app ID feels deeply
-- unsatisfying.
--
-- This case deals with the "Create Issue" button in the context menu.
shouldIgnore ExtractedMessageData {appId = Just "AEMQ3Q4F4"} = True
shouldIgnore _ = False

-- | Inserts todo items to backlink tickets in a message.
handleLinearChannelMessage ::
  (HasApp m, MonadIO m) =>
  Entity Workspace ->
  KnownUserId ->
  JoinedChannelId ->
  ExtractedMessageData ->
  m ()
handleLinearChannelMessage _ _ _ messageData | shouldIgnore messageData = pure ()
handleLinearChannelMessage
  workspaceE@(Entity workspaceId workspace)
  knownUserId
  joinedChannelId
  messageData = do
    let ticketIds = Set.fromList $ extractTicketIds messageData.text
        uniqueTeams = Set.map (.team) ticketIds

    extantTeams <- Set.fromList <$> runDB (knownLinearTeamUrlKeys workspaceId (Set.toList uniqueTeams))

    let plausibleTicketIds = Set.filter ((`Set.member` extantTeams) . (.team)) ticketIds
        workspaceMeta = workspaceMetaFromWorkspaceE workspaceE
        slackUrlParts = extractableMessageToSlackUrlParts workspace.slackSubdomain messageData

    let plausibleTicketIds' = Set.toList plausibleTicketIds
    for_ (NE.nonEmpty plausibleTicketIds')
      $ senderEnqueue
      . BacklinkPlausibleLinearTickets workspaceMeta slackUrlParts joinedChannelId knownUserId
