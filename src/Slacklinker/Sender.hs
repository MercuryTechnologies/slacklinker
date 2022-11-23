module Slacklinker.Sender (
  WorkspaceMeta (..),
  workspaceMetaFromWorkspaceE,
  SenderRequest (..),
  SendMessageReq (..),
  senderEnqueue,
  runSlack,
  senderHandler,
  senderThread,
) where

import Data.Vector qualified as V
import Database.Persist
import Generics.Deriving.ConNames (conNameOf)
import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Context.ThreadLocal qualified as OTel
import OpenTelemetry.Trace (addAttribute)
import OpenTelemetry.Trace.Core qualified as OTel
import Slacklinker.App (AppM, HasApp, appSlackConfig, runDB)
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Slack.ConversationsJoin
import Slacklinker.Types (SlackToken)
import System.IO.Unsafe (unsafePerformIO)
import Web.Slack (SlackConfig, chatPostMessage, conversationsListAll)
import Web.Slack.Chat (PostMsgReq (..), mkPostMsgReq)
import Web.Slack.Common (SlackClientError)
import Web.Slack.Conversation
import Web.Slack.Pager (loadingPage)
import Web.Slack.UsersConversations (usersConversationsAll)

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

senderHandler :: (MonadIO m) => Text -> SomeException -> m ()
senderHandler loc e = do
  case fromException @Terminate e of
    Just _ -> do
      -- should actually quit
      -- FIXME(jadel): monad-logger; needs instance as it may be called under IO
      putStrLn $ loc <> ": Sender terminate"
      throwIO e
    Nothing -> do
      mspan <- OTel.lookupSpan <$> OTel.getContext
      for_ mspan $ \span -> OTel.recordException span [] Nothing e
      -- unexpected exception, log it
      putStrLn $ loc <> ": SENDER EXC: " <> pack (displayException e)

runSlack :: (MonadIO m, HasApp m) => SlackToken -> (SlackConfig -> IO (Either SlackClientError a)) -> m a
runSlack workspaceToken act = do
  slackConfig <- appSlackConfig workspaceToken
  fromEitherIO . liftIO $ act slackConfig

doSendMessage :: (HasApp m, MonadIO m) => SendMessageReq -> m ()
doSendMessage req = do
  let postMsgReq = (mkPostMsgReq req.channel.unConversationId req.messageContent) {postMsgReqThreadTs = req.replyToTs}
  _ <- runSlack req.workspaceMeta.token $ \slackConfig -> chatPostMessage slackConfig postMsgReq
  pure ()

doJoinChannel :: (MonadIO m, HasApp m) => WorkspaceMeta -> ConversationId -> m ()
doJoinChannel ws cid = do
  joinResp <- runSlack ws.token \slackConfig ->
    conversationsJoin slackConfig $ ConversationJoinRequest {channel = cid}
  let name = case joinResp.channel of
        Channel c -> Just c.channelName
        -- This case should never be hit since Slacklinker doesn't operate
        -- in DMs or groups.
        _ -> Nothing
  runDB $ insert_ $ JoinedChannel {workspaceId = ws.workspaceId, channelId = cid, name}
  pure ()

fetchMemberConversations :: (HasApp m, MonadIO m) => WorkspaceMeta -> m (Vector Conversation)
fetchMemberConversations wsInfo = do
  slackConfig <- appSlackConfig wsInfo.token
  list_ <- liftIO $ usersConversationsAll slackConfig def
  liftIO $ loadingPage list_ convertPage
  where
    convertPage resp =
      fromList @(Vector _) <$> fromEither resp

doUpdateJoined :: (HasApp m, MonadIO m) => WorkspaceMeta -> ConversationId -> m ()
doUpdateJoined wsInfo cid = do
  logMessage "Updating metadata"
  doUpdateJoined'
  logMessage "Done!"
  where
    logMessage messageContent = doSendMessage SendMessageReq {replyToTs = Nothing, channel = cid, messageContent, workspaceMeta = wsInfo}

    doUpdateJoined' :: (HasApp m, MonadIO m) => m ()
    doUpdateJoined' = do
      members <- fetchMemberConversations wsInfo
      -- N+1 query, but the alternative is not type safe.
      runDB $ forM_ (V.mapMaybe channelsOnly members) $ \chan -> do
        void $
          upsertBy
            (UniqueJoinedChannel wsInfo.workspaceId chan.channelId)
            JoinedChannel {workspaceId = wsInfo.workspaceId, channelId = chan.channelId, name = Just chan.channelName}
            [JoinedChannelName =. Just chan.channelName]
      where
        channelsOnly (Channel c) = Just c
        channelsOnly _ = Nothing

fetchAllConversations :: (HasApp m, MonadIO m) => WorkspaceMeta -> m (Vector Conversation)
fetchAllConversations wsInfo = do
  slackConfig <- appSlackConfig wsInfo.token
  list_ <-
    liftIO $
      conversationsListAll
        slackConfig
        ( mkListReq
            { listReqExcludeArchived = Just True
            , listReqTypes = [PublicChannelType]
            , listReqLimit = Just 200
            }
        )
  liftIO $ loadingPage list_ convertPage
  where
    -- We exclude shared channels from conversations that we care about, mostly
    -- just to be safe. The bot can be manually joined to such channels if so
    -- desired.
    isShared (Channel c) = channelIsShared c
    isShared _ = False

    convertPage resp =
      fromList @(Vector _) . filter (not . isShared) <$> fromEither resp

doJoinAll :: (HasApp m, MonadIO m) => WorkspaceMeta -> ConversationId -> m ()
doJoinAll wsInfo cid = do
  logMessage "On it! Give me a minute to figure out which channels exist..."

  conversations_ <- fetchAllConversations wsInfo
  let conversations = conversations_ >>= onlyNonMemberChannels
  logMessage $ "Got " <> (tshow $ length conversations) <> " conversations to join. Joining them now."
  forM_ conversations $ \c -> do
    logDebug $ "joining: " <> c.channelName
    senderEnqueue $ JoinChannel wsInfo c.channelId

  logMessage "Done!"
  where
    onlyNonMemberChannels (Channel ch) | ch.channelIsMember /= Just True = singleton ch
    onlyNonMemberChannels _ = empty

    logMessage messageContent = doSendMessage SendMessageReq {replyToTs = Nothing, channel = cid, messageContent, workspaceMeta = wsInfo}

handleTodo :: SenderRequest -> AppM ()
handleTodo r =
  let conName = conNameOf r
   in inSpan' (cs conName) defaultSpanArguments \span -> case r of
        SendMessage req -> do
          addAttribute span "slack.channel" req.channel.unConversationId
          doSendMessage req
        JoinChannel wsInfo cid -> do
          addAttribute span "slack.channel" cid.unConversationId
          addWorkspaceInfo span wsInfo
          doJoinChannel wsInfo cid
        ReqJoinAll wsInfo cid -> do
          addWorkspaceInfo span wsInfo
          doJoinAll wsInfo cid
        ReqUpdateJoined wsInfo cid -> do
          addWorkspaceInfo span wsInfo
          doUpdateJoined wsInfo cid
        RequestTerminate -> throwIO Terminate
  where
    addWorkspaceInfo span wsInfo = addAttribute span "slack.team.id" wsInfo.slackTeamId.unTeamId

senderThread :: AppM ()
senderThread = forever $ do
  SenderEnvelope {request, otelContext} <- atomically $ readTChan senderChan
  OTel.attachContext otelContext

  logDebug $ "SENDER: " <> tshow request

  handleTodo request `catch` senderHandler "senderThread"
