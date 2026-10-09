{-# LANGUAGE NoFieldSelectors #-}

module Slacklinker.Sender.Internal where

import Slacklinker.App (HasApp (..), appSlackConfig)
import Slacklinker.Import
import Slacklinker.Sender.Types
import Slacklinker.SplitUrl (SlackUrlParts (..), buildSlackChannelUrl, buildSlackUrl)
import Slacklinker.Types (SlackToken (..))
import Web.Slack (SlackConfig, chatPostMessage)
import Web.Slack.Chat (PostMsgReq (..), mkPostMsgReq)
import Web.Slack.Common (SlackClientError)
import Web.Slack.Types (ConversationId (..))

{- | Identifies the operation and target of a Slack request for error reporting.

Note that we're careful to not include tokens or message content in these.
-}
data SlackRequestContext
  = -- | Join the specified channel.
    JoinConversation ConversationId
  | -- | Post to a channel, optionally replying to the thread with the given timestamp.
    PostMessage PostMessageContext
  | -- | Update the message identified by its channel and message timestamp.
    UpdateMessage UpdateMessageContext
  deriving stock (Show)

-- | Channel and optional parent thread for a message being posted.
data PostMessageContext = PostMessageContext
  { channel :: ConversationId
  , threadTs :: Maybe Text
  -- ^ Parent message timestamp in Slack's API format, or 'Nothing' for a channel-level post.
  }
  deriving stock (Show)

-- | Channel and message identifying a message being updated.
data UpdateMessageContext = UpdateMessageContext
  { channel :: ConversationId
  , messageTs :: Text
  -- ^ Timestamp of the message being updated, in Slack's API format.
  }
  deriving stock (Show)

{- | A Slack client error annotated with the operation and workspace that failed.

The original error is preserved. 'displayException' includes its description,
the request identifiers, and a link to the request's target.
-}
data SlackRequestError = SlackRequestError
  { workspaceName :: Text
  -- ^ Workspace subdomain, without the @.slack.com@ suffix.
  , requestContext :: SlackRequestContext
  -- ^ Operation and target associated with the failed request.
  , slackError :: SlackClientError
  -- ^ Original client error, retained without modification.
  }
  deriving stock (Show)

instance Exception SlackRequestError where
  displayException err =
    displayException err.slackError
      <> " while "
      <> description
      <> " "
      <> unpack (slackRequestUrl err.workspaceName err.requestContext)
    where
      description = case err.requestContext of
        JoinConversation _channel -> "joining channel"
        PostMessage PostMessageContext {threadTs} ->
          case threadTs of
            Just _ -> "replying to thread"
            Nothing -> "posting message in channel"
        UpdateMessage _ -> "updating message"

{- | Build a browser link to the Slack channel or message involved in a request,
for use in error messages.

The first argument is the workspace subdomain.

For thread replies, links to the parent message; for updates, links to the
message being updated. Falls back to the channel URL when there is no message
timestamp or 'buildSlackUrl' cannot construct a message URL.
-}
slackRequestUrl :: Text -> SlackRequestContext -> Text
slackRequestUrl workspaceName requestContext =
  fromMaybe channelUrl $ do
    messageTs <- targetTs
    buildSlackUrl SlackUrlParts {workspaceName, channelId, messageTs, threadTs = Nothing}
  where
    (channelId, targetTs) = case requestContext of
      JoinConversation channel -> (channel, Nothing)
      PostMessage PostMessageContext {channel, threadTs} -> (channel, threadTs)
      UpdateMessage UpdateMessageContext {channel, messageTs} -> (channel, Just messageTs)
    channelUrl = buildSlackChannelUrl workspaceName channelId

{- | Return a successful response or throw a t'SlackRequestError' for a returned
client error.

The first argument is the workspace subdomain.

Exceptions thrown by the action itself propagate unchanged.
-}
withSlackRequestContext :: (MonadIO m) => Text -> SlackRequestContext -> m (Either SlackClientError a) -> m a
withSlackRequestContext workspaceName context act = fromEitherM $ mapLeft (SlackRequestError workspaceName context) <$> act

{- | Run a Slack API action using the workspace's token, annotating returned
client errors with the workspace subdomain and supplied request context.

The context must describe the operation performed by the action. Successful
responses are returned unchanged; returned client errors are thrown as
t'SlackRequestError'. Use 'runSlackEither' to handle client errors explicitly.
-}
runSlackRequest :: (MonadIO m, HasApp m) => WorkspaceMeta -> SlackRequestContext -> (SlackConfig -> IO (Either SlackClientError a)) -> m a
runSlackRequest workspace context act =
  withSlackRequestContext workspace.slackSubdomain context
    $ runSlackEither workspace.token act

runSlack :: (MonadIO m, HasApp m) => SlackToken -> (SlackConfig -> IO (Either SlackClientError a)) -> m a
runSlack workspaceToken act = fromEitherM $ runSlackEither workspaceToken act

runSlackEither :: (MonadIO m, HasApp m) => SlackToken -> (SlackConfig -> IO (Either SlackClientError a)) -> m (Either SlackClientError a)
runSlackEither workspaceToken act = do
  slackConfig <- appSlackConfig workspaceToken
  liftIO $ act slackConfig

doSendMessage :: (HasApp m, MonadIO m) => SendMessageReq -> m ()
doSendMessage req = do
  let postMsgReq = (mkPostMsgReq req.channel.unConversationId req.messageContent) {postMsgReqThreadTs = req.replyToTs}
  void $ runSlackRequest req.workspaceMeta (PostMessage PostMessageContext {channel = req.channel, threadTs = req.replyToTs}) \slackConfig ->
    chatPostMessage slackConfig postMsgReq
