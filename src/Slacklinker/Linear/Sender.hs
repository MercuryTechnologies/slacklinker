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
import Slacklinker.Linear.GraphQL (runLinearGraphQL)
import Slacklinker.Linear.GraphQL.API (LinkSlackMutation (..))
import Slacklinker.Linear.Types (LinearTicketId, linearTicketIdToText)
import Slacklinker.Models (JoinedChannelId, KnownUserId, LinkedLinearTicket (..))
import Slacklinker.Sender.Internal (runSlackEither)
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
doBacklinkLinearTickets workspaceInfo slackUrlParts joinedChannelId knownUserId tickets = do
  -- The tickets have known linear teams, so we just have to link them.
  slackUrl <- buildSlackUrl slackUrlParts `orThrow` SlacklinkerBug "slack url not successfully reconstituted from a parsed one?!"
  (Value linearOrgId, Value token) <- runDB $ linearAuthSessionForWorkspace workspaceInfo.workspaceId >>= (`orThrow` LinearNotAuthenticated)

  anyLinked <-
    any identity <$> for tickets \ticket -> do
      -- This can fail (e.g. unknown ticket) and it is okay.
      -- FIXME(jadel): maybe we should log such failures?
      resultOrErr <- runLinearGraphQL token (runQuerySafe (LinkSlackMutation {_issueId = linearTicketIdToText ticket, _slackLink = slackUrl}))
      for_ (getResult resultOrErr) \result -> do
        runDB
          $ P.insertBy
            LinkedLinearTicket
              { linearOrganizationId = linearOrgId
              , linearTicketId = [get| result.attachmentLinkSlack.attachment.issue.id |]
              , joinedChannelId = joinedChannelId
              , knownUserId = knownUserId
              , messageTs = slackUrlParts.messageTs
              , threadTs = slackUrlParts.threadTs
              }
      pure . isJust $ getResult resultOrErr

  if anyLinked
    then do
      react "linear"
      react "slacklinker"
    else do
      react "slacklinker"
      react "question"
  where
    react emojiName = do
      result <- runSlackEither workspaceInfo.token \cfg -> do
        Reactions.reactionsAdd
          cfg
          Reactions.AddReq
            { channel = slackUrlParts.channelId
            , timestamp = slackUrlParts.messageTs
            , name = emojiName
            }
      case result of
        Left err -> logWarn $ "Failed to react to message " <> slackUrlParts.messageTs <> " in channel " <> tshow slackUrlParts.channelId <> " with error: " <> tshow err
        Right _ -> pure ()
