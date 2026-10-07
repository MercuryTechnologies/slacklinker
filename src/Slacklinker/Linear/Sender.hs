{-# LANGUAGE QuasiQuotes #-}

module Slacklinker.Linear.Sender (doBacklinkLinearTickets, LinkResult (..), runBacklinkBatch) where

import Data.Foldable1 (Foldable1 (..))
import Data.GraphQL (get, getErrors)
import Data.GraphQL.Error (GraphQLError (..))
import Data.GraphQL.Monad (MonadGraphQLQuery (..))
import Data.GraphQL.Result (getResult)
import Data.Set qualified as Set
import Database.Persist qualified as P
import OpenTelemetry.Trace.Core (Span)
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
import Web.Slack.Common (SlackClientError (..))
import Web.Slack.Reactions qualified as Reactions
import Web.Slack.Types (Emoji)

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
  Span ->
  WorkspaceMeta ->
  SlackUrlParts ->
  JoinedChannelId ->
  KnownUserId ->
  NonEmpty LinearTicketId ->
  m ()
doBacklinkLinearTickets span workspaceInfo slackUrlParts joinedChannelId knownUserId tickets =
  runBacklinkBatch span tickets prepare linkTicket save react
  where
    prepare = do
      -- The tickets have known linear teams, so we just have to link them.
      slackUrl <- buildSlackUrl slackUrlParts `orThrow` SlacklinkerBug "slack url not successfully reconstituted from a parsed one?!"
      (linearOrgId, token) <- getToken workspaceInfo.workspaceId
      pure (slackUrl, linearOrgId, token)

    linkTicket (slackUrl, _, token) ticket = do
      resultOrErr <- runLinearGraphQL token (runQuerySafe (LinkSlackMutation {_issueId = linearTicketIdToText ticket, _slackLink = slackUrl}))
      let result = getResult resultOrErr
          linkingResult
            | isJust result = Created
            | any isDuplicateError (getErrors resultOrErr) = Duplicate
            | otherwise = OtherError $ getErrors resultOrErr
      pure (linkingResult, result)

    save (_, linearOrgId, _) result =
      void
        $ runDB
        $ P.insertBy
          LinkedLinearTicket
            { linearOrganizationId = linearOrgId
            , linearTicketId = [get| result.attachmentLinkSlack.attachment.issue.id |]
            , joinedChannelId = joinedChannelId
            , knownUserId = knownUserId
            , messageTs = slackUrlParts.messageTs
            , threadTs = slackUrlParts.threadTs
            }
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
      pure $ void result

data BatchSummary = BatchSummary
  { attempted :: !Int
  , created :: !Int
  , duplicate :: !Int
  , failed :: !Int
  , completed :: !Bool
  , errors :: !(Set Text)
  , reactionAttempted :: !Int
  , reactionFailed :: !Int
  , reactionErrors :: !(Set Text)
  }

{- | Keep the remote result before persistence and before the lossy user-facing
aggregation. Dependencies are explicit so failure paths can be tested in memory.
All telemetry belongs to the caller's span, including while dependency spans run.
-}
runBacklinkBatch ::
  (MonadUnliftIO m) =>
  Span ->
  NonEmpty ticket ->
  m context ->
  (context -> ticket -> m (LinkResult, Maybe result)) ->
  (context -> result -> m ()) ->
  (Emoji -> m (Either SlackClientError ())) ->
  m ()
runBacklinkBatch span tickets prepare linkTicket save react = do
  summary <- newIORef $ BatchSummary 0 0 0 0 False mempty 0 0 mempty
  let update = modifyIORef' summary
      addError category = update \s -> s {errors = Set.insert category s.errors}
      recordResult = \case
        Created -> update \s -> s {created = s.created + 1}
        Duplicate -> update \s -> s {duplicate = s.duplicate + 1}
        OtherError errs -> update \s ->
          s {failed = s.failed + 1, errors = Set.insert (if null errs then "empty_result" else "graphql_error") s.errors}
      reactionFailure category = update \s ->
        s {reactionFailed = s.reactionFailed + 1, reactionErrors = Set.insert category s.reactionErrors}
      trackedReact emoji = mask \restore -> do
        update \s -> s {reactionAttempted = s.reactionAttempted + 1}
        result <- restore (react emoji) `onException` reactionFailure "reaction_exception"
        case result of
          Left (SlackError _) -> reactionFailure "slack_api_error"
          Left (ServantError _) -> reactionFailure "slack_client_error"
          Right () -> pure ()
      run = do
        context <- prepare `onException` addError "setup_exception"
        linkingResults <- for tickets \ticket -> do
          (result, payload) <- mask \restore -> do
            update \s -> s {attempted = s.attempted + 1}
            linked <-
              restore (linkTicket context ticket) `onException` do
                update \s -> s {failed = s.failed + 1}
                addError "link_exception"
            recordResult $ fst linked
            pure linked
          for_ payload (save context) `onException` addError "persistence_exception"
          pure result
        update \s -> s {completed = True}
        -- Preserve the existing reaction precedence and sequence.
        case fold1 linkingResults of
          Created -> trackedReact "slacklinker-linear"
          OtherError _ -> trackedReact "slacklinker" >> trackedReact "question"
          Duplicate -> pure ()
      record = do
        s <- readIORef summary
        addAttribute span "linear.backlink.tickets.candidates" $ length tickets
        addAttribute span "linear.backlink.tickets.attempted" s.attempted
        addAttribute span "linear.backlink.tickets.created" s.created
        addAttribute span "linear.backlink.tickets.duplicate" s.duplicate
        addAttribute span "linear.backlink.tickets.failed" s.failed
        addAttribute span "linear.backlink.completed" s.completed
        addAttribute span "linear.backlink.outcome" $ batchOutcome s
        addAttribute span "linear.backlink.error_categories" $ Set.toAscList s.errors
        addAttribute span "linear.backlink.reactions.attempted" s.reactionAttempted
        addAttribute span "linear.backlink.reactions.failed" s.reactionFailed
        addAttribute span "linear.backlink.reactions.error_categories" $ Set.toAscList s.reactionErrors
  run `finally` record

batchOutcome :: BatchSummary -> Text
batchOutcome s
  | s.created > 0 = if s.completed && s.failed == 0 then "success" else "partial_success"
  | s.completed && s.failed == 0 = "all_duplicates"
  | otherwise = "failure"
