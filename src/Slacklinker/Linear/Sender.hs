{-# LANGUAGE QuasiQuotes #-}

module Slacklinker.Linear.Sender (doBacklinkLinearTickets) where

import Data.Foldable1 (Foldable1 (..))
import Data.GraphQL (get, getErrors)
import Data.GraphQL.Error (GraphQLError (..))
import Data.GraphQL.Monad (MonadGraphQLQuery (..))
import Data.GraphQL.Result (getResult)
import Database.Persist qualified as P
import Slacklinker.App (HasApp, runDB)
import Slacklinker.Exceptions (SlacklinkerBug (..))
import Slacklinker.Import
import Slacklinker.Linear.GraphQL (runLinearGraphQL)
import Slacklinker.Linear.GraphQL.API (LinkSlackMutation (..))
import Slacklinker.Linear.Session (getToken)
import Slacklinker.Linear.Types (LinearTicketId, linearTicketIdToText)
import Slacklinker.Models (JoinedChannelId, KnownUserId, LinkedLinearTicket (..))
import Slacklinker.Sender.Internal (runSlackEither)
import Slacklinker.Sender.Types (WorkspaceMeta (..))
import Slacklinker.SplitUrl (SlackUrlParts (..), buildSlackUrl)
import Web.Slack.Reactions qualified as Reactions

{- | Regrettably we have no way to really check if this changes and it breaks our
code and this definitely doesn't seem like the stablest way to match it.

See https://linear.app/mercury/issue/DUX-3313 for what this fixes.

This is the error that happens if the given link is already attached:

  {
    "errors": [
      {
        "message": "Duplicate attachment for duplicate url",
        "path": [
          "attachmentLinkSlack"
        ],
        "locations": [
          {
            "line": 3,
            "column": 3
          }
        ],
        "extensions": {
          "type": "invalid input",
          "code": "INPUT_ERROR",
          "statusCode": 400,
          "userError": true,
          "userPresentableMessage": "An attachment with the same URL already exists."
        }
      }
    ],
    "data": null
  }
-}
isDuplicateError :: GraphQLError -> Bool
isDuplicateError (GraphQLError {message = "Duplicate attachment for duplicate url"}) = True
isDuplicateError _ = False

data LinkResult
  = Created
  | Duplicate
  | OtherError [GraphQLError]
  deriving stock (Show)

{- | Precedence hierarchy of linking results we want to report:
1. Always report that we created something.
2. If there are duplicates, we ignore them as if they don't exist, so we
   report other errors (bogus ticket ids) first.
3. If there are only duplicates, we pretend to not have seen the message.
-}
instance Semigroup LinkResult where
  Created <> _ = Created
  _ <> Created = Created
  OtherError errs <> OtherError errs' = OtherError $ errs <> errs'
  OtherError errs <> _ = OtherError errs
  _ <> OtherError errs = OtherError errs
  Duplicate <> Duplicate = Duplicate

doBacklinkLinearTickets ::
  (MonadUnliftIO m, HasApp m) =>
  WorkspaceMeta ->
  SlackUrlParts ->
  JoinedChannelId ->
  KnownUserId ->
  NonEmpty LinearTicketId ->
  m ()
doBacklinkLinearTickets workspaceInfo slackUrlParts joinedChannelId knownUserId tickets = do
  -- The tickets have known linear teams, so we just have to link them.
  slackUrl <- buildSlackUrl slackUrlParts `orThrow` SlacklinkerBug "slack url not successfully reconstituted from a parsed one?!"
  (linearOrgId, token) <- getToken workspaceInfo.workspaceId

  linkingResults <- for tickets \ticket -> do
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
    pure
      $ if
        | isJust (getResult resultOrErr) -> Created
        | any isDuplicateError (getErrors resultOrErr) -> Duplicate
        | otherwise -> OtherError $ getErrors resultOrErr

  let overallLinkingResult = fold1 linkingResults

  case overallLinkingResult of
    Created -> do
      react "slacklinker-linear"
    -- Errors; probably just bogus ticket IDs
    (OtherError _errs) -> do
      react "slacklinker"
      react "question"
    -- Only had duplicate ones, so we don't report having done anything
    Duplicate -> pure ()
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
