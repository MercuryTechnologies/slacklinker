{- | == Design of Slacklinker webhooks

   Slacklinker uses the [Slack Events API](https://api.slack.com/events) to
   get real time notifications of messages.

   Webhooks need to have their signature verified, which is done on the
   endpoint handler. After this, we know that the messages were sent by Slack
   and can be trusted.
-}
module Slacklinker.Handler.Webhook (postSlackInteractiveWebhookR) where

import Data.Aeson (Result (..), Value (Object), (.:), (.:?))
import Data.Aeson.Types (parse)
import Data.Text qualified as T
import Database.Persist
import Generics.Deriving.ConNames (conNameOf)
import OpenTelemetry.Trace.Core (Span, addAttribute)
import Slacklinker.App
import Slacklinker.Exceptions
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender
import Slacklinker.SplitUrl
import Web.Slack.Conversation (ConversationId (..))
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Experimental.RequestVerification (SlackRequestTimestamp, SlackSignature, validateRequest)
import Web.Slack.Types (TeamId (..))

extractLinks :: SlackBlock -> [Text]
extractLinks block =
  fromBlock block
  where
    fromBlock (SlackBlockRichText rt) = fromRichText rt
    fromBlock _ = []

    fromRichText rt = mconcat $ fromRichSectionItem <$> rt.elements
    fromRichSectionItem (RichTextSectionItemRichText rt) = mconcat $ fromRichItem <$> rt
    fromRichSectionItem _ = []

    fromRichItem (RichItemLink RichLinkAttrs {..}) = [url]
    fromRichItem _ = []

workspaceMetaFromWorkspaceE :: Entity Workspace -> WorkspaceMeta
workspaceMetaFromWorkspaceE (Entity wsId ws) =
  WorkspaceMeta
    { slackTeamId = ws.slackTeamId
    , workspaceId = wsId
    , token = ws.slackOauthToken
    }

makeMessage :: Entity Workspace -> MessageEvent -> SlackUrlParts -> Maybe SendMessageReq
makeMessage wsE@(Entity _ ws) msgEv SlackUrlParts {..} = do
  guard $ not linkedMessageIsInSameThread
  messageContent <- mMessageContent
  pure
    SendMessageReq
      { replyToTs = Just messageTs
      , channel = channelId
      , messageContent
      , workspaceMeta = workspaceMetaFromWorkspaceE wsE
      }
  where
    -- If you link a message in the same thread as it is in, it doesn't make
    -- any sense to reply to that thread since it will just add noise.
    linkedMessageIsInSameThread = Just messageTs == msgEv.threadTs

    referencerSUP =
      SlackUrlParts
        { channelId = msgEv.channel
        , messageTs = msgEv.ts
        , threadTs = msgEv.threadTs
        }
    mMessageContent = ("This message was referenced elsewhere: " <>) <$> buildSlackUrl ws.slackSubdomain referencerSUP

workspaceByTeamId :: (HasApp m, MonadIO m) => TeamId -> m (Entity Workspace)
workspaceByTeamId teamId = (runDB $ getBy $ UniqueWorkspaceSlackId teamId) >>= (`orThrow` UnknownWorkspace teamId)

data ImCommand
  = Help
  | JoinAll
  deriving stock (Show)

parseImCommand :: Text -> ImCommand
parseImCommand rawText = go $ T.strip rawText
  where
    go "help" = Help
    go "join_all" = JoinAll
    go _ = Help

helpMessage :: Text
helpMessage =
  unlines
    [ "Welcome to Slacklinker. Here are the commands you can use:"
    , "* `join_all` - Join all public channels"
    ]

handleMessage :: (HasApp m, MonadIO m) => MessageEvent -> TeamId -> m ()
handleMessage ev teamId = do
  workspace <- workspaceByTeamId teamId
  case ev.channelType of
    Channel -> do
      let links = mconcat $ extractLinks <$> ev.blocks
          todos = mapMaybe (\url -> makeMessage workspace ev =<< splitSlackUrl url) links
      -- this is like a n+1 query of STM, which is maybe bad for perf vs running
      -- it one action, but whatever
      forM_ todos $ \todo -> do
        senderEnqueue $ SendMessage todo
    Im -> do
      let cmd = parseImCommand ev.text
      case cmd of
        Help ->
          senderEnqueue . SendMessage $
            SendMessageReq
              { replyToTs = Nothing
              , channel = ev.channel
              , messageContent = helpMessage
              , workspaceMeta = workspaceMetaFromWorkspaceE workspace
              }
        JoinAll -> senderEnqueue $ ReqJoinAll (workspaceMetaFromWorkspaceE workspace) ev.channel

      pure ()
    Group ->
      -- we don't do these
      pure ()

handleCallback :: Event -> TeamId -> Span -> AppM Value
handleCallback (EventMessage ev) teamId span | isNothing ev.botId = do
  addAttribute span "slack.conversation.id" ev.channel.unConversationId
  handleMessage ev teamId
  pure $ Object mempty
-- if it's a bot message
handleCallback (EventMessage _ev) _ _ = pure $ Object mempty
handleCallback (EventMessageChanged) _ _ = pure $ Object mempty
handleCallback (EventChannelJoinMessage) _ _ = pure $ Object mempty
handleCallback (EventChannelCreated createdEvent) teamId _ = do
  -- join new channels
  -- FIXME(jadel): should this be configurable behaviour?
  Entity workspaceId workspace <- workspaceByTeamId teamId
  senderEnqueue $
    JoinChannel
      WorkspaceMeta
        { slackTeamId = workspace.slackTeamId
        , token = workspace.slackOauthToken
        , workspaceId
        }
      createdEvent.channel.id
  pure $ Object mempty
handleCallback (EventChannelLeft l) teamId _ = do
  -- remove our database entry stating we're in it
  Entity wsId _ <- workspaceByTeamId teamId
  runDB $ do
    deleteBy $ UniqueJoinedChannel wsId l.channel
  pure $ Object mempty
handleCallback (EventUnknown v) _ span = do
  case parse typeAndSubtype v of
    Success (type_, subtype) -> do
      unless (isUnknownButIgnored type_ subtype) logUnknown
      addAttribute span "slack.event.type" type_
      addAttribute span "slack.event.subtype" (fromMaybe "" subtype)
    _ -> logUnknown
  pure $ Object mempty
  where
    logUnknown = logInfo $ "unknown webhook callback: " <> tshow v
    typeAndSubtype = withObject "webhook event" \val -> do
      type_ <- val .: "type"
      subtype <- val .:? "subtype"
      pure (type_, subtype)

    isUnknownButIgnored :: Text -> Maybe Text -> Bool
    isUnknownButIgnored "message" (Just "bot_message") = True
    isUnknownButIgnored "message" (Just "message_deleted") = True
    isUnknownButIgnored "message" (Just "channel_purpose") = True
    isUnknownButIgnored _ _ = False

handleEvent :: SlackWebhookEvent -> AppM Value
handleEvent (EventUrlVerification UrlVerificationPayload {..}) = do
  pure . toJSON $ UrlVerificationResponse {challenge}
handleEvent (EventEventCallback EventCallback {event, teamId}) = do
  inSpan' (cs $ conNameOf event) defaultSpanArguments \span -> do
    addAttribute span "slack.team.id" teamId.unTeamId
    handleCallback event teamId span
handleEvent (EventUnknownWebhook v) = do
  logInfo $ "unknown webhook event: " <> tshow v
  pure $ Object mempty

postSlackInteractiveWebhookR :: SlackSignature -> SlackRequestTimestamp -> ByteString -> AppM Value
postSlackInteractiveWebhookR sig ts body = do
  secret <- getsApp (.config.slackSigningSecret)
  ePayload <- validateRequest secret sig ts body
  case ePayload of
    Left err -> do
      logDebug $ "webhook err: " <> tshow err
      throwIO $ VerificationException err
    Right todo -> do
      logDebug $ "payload: " <> cs body <> "\n\n"
      logDebug $ "webhook todo: " <> tshow todo <> "\n\n"
      inSpan (cs $ conNameOf todo) defaultSpanArguments $ do
        handleEvent todo
