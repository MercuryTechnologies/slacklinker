{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Linear.Sender (doBacklinkLinearTickets) where

import Data.GraphQL (get)
import Data.GraphQL.Monad (MonadGraphQLQuery (..))
import Data.GraphQL.Result (getResult)
import Database.Esqueleto.Experimental (Value (..))
import Database.Persist qualified as P
import Slacklinker.App (HasApp, runDB)
import Slacklinker.Exceptions (LinearNotAuthenticated (..), SlacklinkerBug (..))
import Slacklinker.Import
import Slacklinker.Linear.DB (linearAuthSessionForWorkspace)
import Slacklinker.Linear.GraphQL (runQuery)
import Slacklinker.Linear.GraphQL.API (LinkSlackMutation (..))
import Slacklinker.Linear.Types (LinearTicketId, linearTicketIdToText)
import Slacklinker.Models (JoinedChannelId, KnownUserId, LinkedLinearTicket (..))
import Slacklinker.Sender.Internal (runSlack')
import Slacklinker.Sender.Types (WorkspaceMeta (..))
import Slacklinker.SplitUrl (SlackUrlParts (..), buildSlackUrl)
import Web.Slack.Reactions qualified as Reactions

doBacklinkLinearTickets ::
  (MonadIO m, HasApp m) =>
  WorkspaceMeta ->
  SlackUrlParts ->
  JoinedChannelId ->
  KnownUserId ->
  [LinearTicketId] ->
  m ()
doBacklinkLinearTickets wsInfo sup jcid kuid tickets = do
  -- The tickets have known linear teams, so we just have to link them.
  slackUrl <- buildSlackUrl sup `orThrow` SlacklinkerBug "slack url not successfully reconstituted from a parsed one?!"
  (Value loId, Value token) <- runDB $ linearAuthSessionForWorkspace wsInfo.workspaceId >>= (`orThrow` LinearNotAuthenticated)

  anyLinked <-
    any identity <$> for tickets \ticket -> do
      res <- runQuery token (runQuerySafe (LinkSlackMutation {_issueId = linearTicketIdToText ticket, _slackLink = slackUrl}))
      for_ (getResult res) \r -> do
        runDB
          $ P.insertBy
            LinkedLinearTicket
              { linearOrganizationId = loId
              , linearTicketId = [get| r.attachmentLinkSlack.attachment.issue.id |]
              , joinedChannelId = jcid
              , knownUserId = kuid
              , messageTs = sup.messageTs
              , threadTs = sup.threadTs
              }
      pure . isJust $ getResult res

  if anyLinked
    then do
      react "linear"
      react "slacklinker"
    else do
      react "slacklinker"
      react "question"
  where
    react emojiName = do
      r <- runSlack' wsInfo.token \cfg -> do
        Reactions.reactionsAdd cfg Reactions.AddReq {channel = sup.channelId, timestamp = sup.messageTs, name = emojiName}
      case r of
        Left err -> logWarn $ "Failed to react to message " <> sup.messageTs <> " in channel " <> tshow sup.channelId <> " with error: " <> tshow err
        Right _ -> pure ()
