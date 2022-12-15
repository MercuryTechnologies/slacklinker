module Slacklinker.Sender.Types where

import Database.Persist
import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Context.ThreadLocal qualified as OTel
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Types (SlackToken (..))
import System.IO.Unsafe (unsafePerformIO)
import Web.Slack.Conversation
import Web.Slack.Files.Types (FileObject (..))

data WorkspaceMeta = WorkspaceMeta
  { token :: SlackToken
  , workspaceId :: WorkspaceId
  , slackTeamId :: TeamId
  }
  deriving stock (Show)

workspaceMetaFromWorkspaceE :: Entity Workspace -> WorkspaceMeta
workspaceMetaFromWorkspaceE (Entity wsId ws) =
  WorkspaceMeta
    { slackTeamId = ws.slackTeamId
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

data SenderEnvelope = SenderEnvelope
  { otelContext :: OTel.Context
  , request :: SenderRequest
  }

mkSenderEnvelope :: MonadIO m => SenderRequest -> m SenderEnvelope
mkSenderEnvelope request = do
  otelContext <- OTel.getContext
  pure SenderEnvelope {otelContext, request}

data SenderRequest
  = SendMessage SendMessageReq
  | JoinChannel WorkspaceMeta ConversationId
  | -- | Join all public non-archived channels in the workspace. The provided
    -- ConversationId is for feedback to the user.
    ReqJoinAll WorkspaceMeta ConversationId
  | -- | Update joined channel metadata. Required to make link previews include
    -- channels. This is a one-off fixup: when Slacklinker joins new channels
    -- with join_all, it will gather this metadata.
    --
    -- FIXME(jadel): If you manually join slacklinker to a channel, the metadata
    -- will probably also be outdated? Maybe we need to subscribe to
    -- <https://api.slack.com/events/member_joined_channel> and add a new
    -- scheduled task?
    ReqUpdateJoined WorkspaceMeta ConversationId
  | UpdateReply RepliedThreadId
  | ReqUploadUserData WorkspaceMeta ConversationId FileObject
  | RequestTerminate
  deriving stock (Show, Generic)

data Terminate = Terminate deriving stock (Show)

instance Exception Terminate

senderEnqueue :: MonadIO m => SenderRequest -> m ()
senderEnqueue req = do
  message <- mkSenderEnvelope req
  atomically $ writeTChan senderChan message

{-# NOINLINE senderChan #-}
senderChan :: TChan SenderEnvelope
senderChan = unsafePerformIO newTChanIO
