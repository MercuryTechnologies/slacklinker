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

import Data.HashMap.Strict qualified as HM
import Data.Text (splitOn)
import Data.Vector qualified as V
import Database.Esqueleto.Experimental qualified as E
import Database.Persist
import Generics.Deriving.ConNames (conNameOf)
import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Context.ThreadLocal qualified as OTel
import OpenTelemetry.Trace (addAttribute)
import OpenTelemetry.Trace.Core qualified as OTel
import Slacklinker.App (AppM, HasApp (..), appSlackConfig, runDB)
import Slacklinker.Exceptions (SlacklinkerBug (..))
import Slacklinker.Import
import Slacklinker.Linear.Sender (doBacklinkLinearTickets)
import Slacklinker.Linear.Teams (updateLinearTeamsCache)
import Slacklinker.Models
import Slacklinker.Sender.AppHome
import Slacklinker.Sender.Internal
import Slacklinker.Sender.Types
import Slacklinker.Sender.UserDataUpload
import Slacklinker.Slack.ConversationsJoin
import Slacklinker.SplitUrl (SlackUrlParts (..), buildSlackUrl)
import Slacklinker.Types (Emoji (..))
import Slacklinker.UpdateReply.Sql (linkedMessagesInThread, workspaceByRepliedThreadId)
import Web.Slack (chatPostMessage, chatUpdate, conversationsListAll)
import Web.Slack.Chat (PostMsgReq (..), PostMsgRsp (..), UpdateReq (..), UpdateRsp (..), mkPostMsgReq, mkUpdateReq)
import Web.Slack.Conversation
import Web.Slack.Pager (loadingPage)
import Web.Slack.UsersConversations (usersConversationsAll)

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
      for_ mspan $ \span -> OTel.recordException span HM.empty Nothing e
      -- unexpected exception, log it
      putStrLn $ loc <> ": SENDER EXC: " <> pack (displayException e)

doJoinChannel :: (MonadIO m, HasApp m) => WorkspaceMeta -> ConversationId -> m ()
doJoinChannel ws conversation = do
  joinResp <- runSlack ws.token \slackConfig ->
    conversationsJoin slackConfig $ ConversationJoinRequest {channel = conversation}
  let name = case joinResp.channel of
        Channel c -> Just c.channelName
        -- This case should never be hit since Slacklinker doesn't operate
        -- in DMs or groups.
        _ -> Nothing
  runDB $ insert_ $ JoinedChannel {workspaceId = ws.workspaceId, channelId = conversation, name}
  pure ()

fetchMemberConversations :: (HasApp m, MonadIO m) => WorkspaceMeta -> m (Vector Conversation)
fetchMemberConversations workspaceInfo = do
  slackConfig <- appSlackConfig workspaceInfo.token
  list_ <- liftIO $ usersConversationsAll slackConfig def
  liftIO $ loadingPage list_ convertPage
  where
    convertPage resp =
      fromList @(Vector _) <$> fromEither resp

doUpdateJoined :: (HasApp m, MonadIO m) => WorkspaceMeta -> ConversationId -> m ()
doUpdateJoined workspaceInfo conversation = do
  logMessage "Updating metadata"
  doUpdateJoined'
  logMessage "Done!"
  where
    logMessage messageContent = doSendMessage SendMessageReq {replyToTs = Nothing, channel = conversation, messageContent, workspaceMeta = workspaceInfo}

    doUpdateJoined' :: (HasApp m, MonadIO m) => m ()
    doUpdateJoined' = do
      members <- fetchMemberConversations workspaceInfo
      -- N+1 query, but the alternative is not type safe.
      runDB $ forM_ (V.mapMaybe channelsOnly members) $ \chan -> do
        void
          $ upsertBy
            (UniqueJoinedChannel workspaceInfo.workspaceId chan.channelId)
            JoinedChannel {workspaceId = workspaceInfo.workspaceId, channelId = chan.channelId, name = Just chan.channelName}
            [JoinedChannelName =. Just chan.channelName]
      where
        channelsOnly (Channel c) = Just c
        channelsOnly _ = Nothing

fetchAllConversations :: (HasApp m, MonadIO m) => WorkspaceMeta -> m (Vector Conversation)
fetchAllConversations workspaceInfo = do
  slackConfig <- appSlackConfig workspaceInfo.token
  list_ <-
    liftIO
      $ conversationsListAll
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
doJoinAll workspaceInfo conversation = do
  logMessage "On it! Give me a minute to figure out which channels exist..."

  conversations_ <- fetchAllConversations workspaceInfo
  let conversations = conversations_ >>= onlyNonMemberChannels
  logMessage $ "Got " <> (tshow $ length conversations) <> " conversations to join. Joining them now."
  forM_ conversations $ \c -> do
    logDebug $ "joining: " <> c.channelName
    senderEnqueue $ JoinChannel workspaceInfo c.channelId

  logMessage "Done!"
  where
    onlyNonMemberChannels (Channel ch) | ch.channelIsMember /= Just True = singleton ch
    onlyNonMemberChannels _ = empty

    logMessage messageContent = doSendMessage SendMessageReq {replyToTs = Nothing, channel = conversation, messageContent, workspaceMeta = workspaceInfo}

draftMessage :: Workspace -> [(LinkedMessage, Maybe KnownUser, JoinedChannel)] -> Text
draftMessage workspace links =
  let linksText = mapMaybe toLink links
   in makeMessage linksText
  where
    makeUrl :: Maybe KnownUser -> Maybe Text -> SlackUrlParts -> Maybe Text
    makeUrl mUser mChannelName urlParts = do
      url <- buildSlackUrl urlParts
      let mEmoji = mUser >>= (.emoji) <&> unEmoji
      pure $ case mChannelName of
        Just channelName -> case mEmoji of
          Just emoji ->
            concat ["<", url, "|:", emoji, ": in #", channelName, ">, ", mkDate urlParts.messageTs]
          Nothing ->
            concat ["<", url, "|In #", channelName, ">, ", mkDate urlParts.messageTs]
        Nothing -> url

    -- https://api.slack.com/reference/surfaces/formatting#date-formatting
    mkDate ts =
      let
        -- Slack doesn't like decimals in dates
        extractFirst (x : _) = x
        extractFirst _ = ""
        ts' = extractFirst $ splitOn "." ts
       in
        concat ["<!date^", ts', "^{date_short_pretty} at {time}|datetime>"]

    toLink :: (LinkedMessage, Maybe KnownUser, JoinedChannel) -> Maybe Text
    toLink (LinkedMessage {messageTs, threadTs}, mUser, joinedChannel) =
      makeUrl
        mUser
        joinedChannel.name
        SlackUrlParts {workspaceName = workspace.slackSubdomain, messageTs, channelId = joinedChannel.channelId, threadTs}

    makeMessage :: [Text] -> Text
    makeMessage linksToInclude =
      let linksList = unlines $ map ("• " <>) linksToInclude
       in "This thread was linked elsewhere (mouse over for preview):\n" <> linksList

doUpdateReply :: (HasApp m, MonadIO m) => RepliedThreadId -> m ()
doUpdateReply r = do
  -- FIXME(jadel): do locking in case someone happens to run a high
  -- availability slack bot cluster
  (repliedThread, workspace, links) <- runDB $ do
    repliedThread <- getJust r
    links <- E.select $ linkedMessagesInThread r
    Entity _ workspace <-
      E.selectOne (workspaceByRepliedThreadId r)
        >>= flip orThrow (SlacklinkerBug "workspace does not exist for a replied thread ID")
    pure (repliedThread, workspace, links)

  let message = draftMessage workspace (map (\(lm, mU, jc) -> (entityVal lm, entityVal <$> mU, entityVal jc)) links)

  ts <-
    sendOrReplaceSlackMessage
      workspace.slackOauthToken
      (repliedThread.conversationId, repliedThread.threadTs, repliedThread.replyTs)
      message

  runDB $ do
    update r [RepliedThreadReplyTs =. Just ts]
    updateWhere
      [LinkedMessageId <-. ((\(x, _, _) -> entityKey x) <$> links)]
      [LinkedMessageSent =. True]
  where
    sendOrReplaceSlackMessage token (conversationId, _threadTs, Just ts) content =
      updateRspTs <$> runSlack token \slackConfig ->
        chatUpdate
          slackConfig
          ( (mkUpdateReq conversationId ts)
              { updateReqText = Just content
              }
          )
    sendOrReplaceSlackMessage token (conversationId, threadTs, Nothing) content =
      postMsgRspTs <$> runSlack token \slackConfig ->
        chatPostMessage
          slackConfig
          ( (mkPostMsgReq conversationId.unConversationId content)
              { postMsgReqThreadTs = Just threadTs
              , postMsgReqUnfurlLinks = Just False
              }
          )

handleTodo :: SenderRequest -> AppM ()
handleTodo r =
  let conName = conNameOf r
   in inSpan' (cs conName) defaultSpanArguments \span -> case r of
        SendMessage req -> do
          addAttribute span "slack.channel" req.channel.unConversationId
          doSendMessage req
        JoinChannel workspaceInfo conversation -> do
          addAttribute span "slack.channel" conversation.unConversationId
          addWorkspaceInfo span workspaceInfo
          doJoinChannel workspaceInfo conversation
        ReqJoinAll workspaceInfo conversation -> do
          addWorkspaceInfo span workspaceInfo
          doJoinAll workspaceInfo conversation
        ReqUpdateJoined workspaceInfo conversation -> do
          addWorkspaceInfo span workspaceInfo
          doUpdateJoined workspaceInfo conversation
        UpdateReply replyId -> do
          doUpdateReply replyId
        ReqUploadUserData workspaceInfo conversation files -> do
          doUploadUserData workspaceInfo conversation files
        UpdateAppHome workspaceInfo user -> do
          doUpdateUser'sAppHome workspaceInfo user
        ReqUpdateLinearTeams workspaceInfo conversation -> do
          updateLinearTeamsCache workspaceInfo.workspaceId
          doSendMessage
            SendMessageReq
              { replyToTs = Nothing
              , channel = conversation
              , messageContent = "Done!"
              , workspaceMeta = workspaceInfo
              }
        BacklinkPlausibleLinearTickets workspaceInfo slackUrlParts joinedChannelId knownUserId tickets ->
          doBacklinkLinearTickets workspaceInfo slackUrlParts joinedChannelId knownUserId tickets
        RequestTerminate -> throwIO Terminate
  where
    addWorkspaceInfo span workspaceInfo = addAttribute span "slack.team.id" workspaceInfo.slackTeamId.unTeamId

senderThread :: AppM ()
senderThread = forever $ do
  SenderEnvelope {request, otelContext} <- atomically $ readTChan senderChan
  void $ OTel.attachContext otelContext

  logDebug $ "SENDER: " <> tshow request

  handleTodo request `catch` senderHandler "senderThread"
