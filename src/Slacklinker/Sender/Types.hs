module Slacklinker.Sender.Types where

import Database.Persist
import GHC.Clock (getMonotonicTimeNSec)
import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Context.ThreadLocal qualified as OTel
import Slacklinker.Import
import Slacklinker.Linear.Types (LinearTicketId)
import Slacklinker.Models
import Slacklinker.SplitUrl (SlackUrlParts)
import Slacklinker.Types (SlackToken (..))
import System.IO.Unsafe (unsafePerformIO)
import Web.Slack.Conversation
import Web.Slack.Files.Types (FileObject (..))
import Web.Slack.Types (UserId)

data WorkspaceMeta = WorkspaceMeta
  { token :: SlackToken
  , workspaceId :: WorkspaceId
  , slackTeamId :: TeamId
  , slackSubdomain :: Text
  -- ^ Workspace subdomain, without @.slack.com@, used to build request error links.
  }
  deriving stock (Show)

workspaceMetaFromWorkspaceE :: Entity Workspace -> WorkspaceMeta
workspaceMetaFromWorkspaceE (Entity wsId ws) =
  WorkspaceMeta
    { slackTeamId = ws.slackTeamId
    , slackSubdomain = ws.slackSubdomain
    , workspaceId = wsId
    , token = ws.slackOauthToken
    }

data SendMessageReq = SendMessageReq
  { replyToTs :: Maybe Text
  , channel :: ConversationId
  , messageContent :: Text
  , workspaceMeta :: WorkspaceMeta
  }
  deriving stock (Show)

-- | An in-process job with its originating trace context and queue timing.
data SenderEnvelope = SenderEnvelope
  { otelContext :: OTel.Context
  , request :: SenderRequest
  , enqueuedAtNSec :: Word64
  -- ^ Monotonic nanoseconds sampled after context capture, before queue insertion.
  }

-- | Capture context and then the enqueue timestamp; insert the result immediately.
mkSenderEnvelope :: (MonadIO m) => SenderRequest -> m SenderEnvelope
mkSenderEnvelope request = do
  otelContext <- OTel.getContext
  enqueuedAtNSec <- liftIO getMonotonicTimeNSec
  pure SenderEnvelope {otelContext, request, enqueuedAtNSec}

{- | Convert enqueue and dequeue monotonic nanoseconds to milliseconds.
Subtract before converting to preserve short waits at large clock readings.
Both readings must come from the same process, in enqueue/dequeue order.
-}
queueWaitMilliseconds :: Word64 -> Word64 -> Double
queueWaitMilliseconds enqueuedAtNSec dequeuedAtNSec =
  fromIntegral (dequeuedAtNSec - enqueuedAtNSec) / 1_000_000

data SenderRequest
  = SendMessage SendMessageReq
  | JoinChannel WorkspaceMeta ConversationId
  | {- | Join all public non-archived channels in the workspace. The provided
    ConversationId is for feedback to the user.
    -}
    ReqJoinAll WorkspaceMeta ConversationId
  | {- | Update joined channel metadata. Required to make link previews include
    channels. This is a one-off fixup: when Slacklinker joins new channels
    with join_all, it will gather this metadata.

    FIXME(jadel): If you manually join slacklinker to a channel, the metadata
    will probably also be outdated? Maybe we need to subscribe to
    <https://api.slack.com/events/member_joined_channel> and add a new
    scheduled task?
    -}
    ReqUpdateJoined WorkspaceMeta ConversationId
  | UpdateReply RepliedThreadId
  | ReqUploadUserData WorkspaceMeta ConversationId FileObject
  | {- | Update the given user's App Home view. Triggered by the @app_home_opened@ event from Slack.

    <https://api.slack.com/events/app_home_opened>
    -}
    UpdateAppHome WorkspaceMeta UserId
  | {- | Update the cache of Linear teams for the workspace and log to the
    given conversation that it is done.
    -}
    ReqUpdateLinearTeams WorkspaceMeta ConversationId
  | {- | Backlink something that looks like a Linear ticket (and we know the
    Linear team probably exists for).

    Takes the Slack message URL that generated it.
    -}
    BacklinkPlausibleLinearTickets WorkspaceMeta SlackUrlParts JoinedChannelId KnownUserId (NonEmpty LinearTicketId)
  | RequestTerminate
  deriving stock (Show, Generic)

data Terminate = Terminate deriving stock (Show)

instance Exception Terminate

{- | Enqueue a job with its current trace context. Queue wait starts at the
monotonic clock read in 'mkSenderEnvelope', after context capture and immediately
before the write transaction. It includes transaction and scheduling overhead
until the clock read immediately after dequeue, but excludes job execution.
-}
senderEnqueue :: (MonadIO m) => SenderRequest -> m ()
senderEnqueue req = do
  message <- mkSenderEnvelope req
  atomically $ writeTChan senderChan message

{-# NOINLINE senderChan #-}
senderChan :: TChan SenderEnvelope
senderChan = unsafePerformIO newTChanIO
