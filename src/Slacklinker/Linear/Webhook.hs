module Slacklinker.Linear.Webhook (shouldIgnore, handleLinearChannelMessage) where

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
shouldIgnore _ = False

-- | Inserts todo items to backlink tickets in a message.
handleLinearChannelMessage ::
  (HasApp m, MonadIO m) =>
  Entity Workspace ->
  KnownUserId ->
  JoinedChannelId ->
  ExtractedMessageData ->
  m ()
handleLinearChannelMessage _ _ _ ev | shouldIgnore ev = pure ()
handleLinearChannelMessage workspaceE@(Entity wsId ws) kuid jcid ev = do
  let ticketIds = Set.fromList $ extractTicketIds ev.text
      uniqueTeams = Set.map (.team) ticketIds

  extantTeams <- Set.fromList <$> runDB (knownLinearTeamUrlKeys wsId (Set.toList uniqueTeams))

  let plausibleTicketIds = Set.filter ((`Set.member` extantTeams) . (.team)) ticketIds
      wsMeta = workspaceMetaFromWorkspaceE workspaceE
      sup = extractableMessageToSlackUrlParts ws.slackSubdomain ev

  let plausibleTicketIds' = Set.toList plausibleTicketIds
  unless (null plausibleTicketIds)
    . senderEnqueue
    $ BacklinkPlausibleLinearTickets wsMeta sup jcid kuid plausibleTicketIds'
