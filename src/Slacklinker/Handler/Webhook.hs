{- | == Design of Slacklinker webhooks

   Slacklinker uses the [Slack Events API](https://api.slack.com/events) to
   get real time notifications of messages.

   Webhooks need to have their signature verified, which is done on the
   endpoint handler. After this, we know that the messages were sent by Slack
   and can be trusted.
-}
module Slacklinker.Handler.Webhook (postSlackInteractiveWebhookR, handleMessage) where

import Control.Monad.Extra (mapMaybeM)
import Data.Aeson (Result (..), Value (Object), (.:), (.:?))
import Data.Aeson.Types (Parser, parse, fromJSON)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Database.Persist
import Generics.Deriving.ConNames (conNameOf)
import OpenTelemetry.Trace.Core (Span, addAttribute)
import Slacklinker.App
import Slacklinker.Exceptions
import Slacklinker.Handler.Webhook.ImCommand (handleImCommand)
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender
import Slacklinker.SplitUrl
import Web.Slack.Conversation (ConversationId (..))
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Experimental.RequestVerification (SlackRequestTimestamp, SlackSignature, SlackVerificationFailed (..), validateRequest)
import Web.Slack.Types (TeamId (..))
import Web.Slack.Types qualified as Slack (UserId (..))

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

data MessageDestination = MessageDestination
  { replyToTs :: Maybe Text
  , channel :: ConversationId
  , workspaceMeta :: WorkspaceMeta
  }
  deriving stock (Show)

-- | Records that a message has been linked to
recordLink ::
  (HasApp m, MonadIO m) =>
  WorkspaceId ->
  KnownUserId ->
  SlackUrlParts ->
  SlackUrlParts ->
  m (Maybe RepliedThreadId)
recordLink workspaceId userId linkSource linkDestination = do
  if isInSameThread linkSource linkDestination
    then pure Nothing
    else do
      runDB $ do
        repliedThreadId_ <-
          insertBy
            RepliedThread
              { workspaceId
              , replyTs = Nothing
              , -- Destination of the link
                conversationId = linkDestination.channelId
              , -- If a link is to a message in a thread, the message we're
                -- going to reply to is the thread parent. If it's a standalone
                -- message, it's the message itself. This deduplicates our
                -- replies between messages within the same thread.
                threadTs =
                  fromMaybe
                    linkDestination.messageTs
                    linkDestination.threadTs
              }
        let repliedThreadId = either entityKey identity repliedThreadId_

        joinedChannelId_ <-
          insertBy
            JoinedChannel {workspaceId, name = Nothing, channelId = linkSource.channelId}
        let joinedChannelId = either entityKey identity joinedChannelId_

        -- We ignore unique violations here on purpose: if it's already been noted,
        -- we don't care.
        void $
          insertBy
            LinkedMessage
              { repliedThreadId
              , -- The message event is the source of the link
                joinedChannelId
              , knownUserId = Just userId
              , messageTs = linkSource.messageTs
              , threadTs = linkSource.threadTs
              , sent = False
              }
        pure $ Just repliedThreadId
  where
    isJustAndEqual a b = fromMaybe False (liftM2 (==) a b)
    isInSameThread link1 link2 =
      link1.channelId == link2.channelId
        && (
             -- link2 is the thread parent of link1
             link1.threadTs == Just link2.messageTs
              ||
              -- link1 is the thread parent of link2
              link2.threadTs == Just link1.messageTs
              -- both are children of the same thread
              || (link1.threadTs `isJustAndEqual` link2.threadTs)
           )

recordUser :: (HasApp m, MonadIO m) => WorkspaceId -> Slack.UserId -> m KnownUserId
recordUser workspaceId slackUserId = do
  let user = KnownUser {workspaceId, slackUserId, emoji = Nothing, email = Nothing, githubUsername = Nothing}
  runDB (insertBy user) >>= either (pure . entityKey) pure

workspaceByTeamId :: (HasApp m, MonadIO m) => TeamId -> m (Entity Workspace)
workspaceByTeamId teamId = (runDB $ getBy $ UniqueWorkspaceSlackId teamId) >>= (`orThrow` UnknownWorkspace teamId)

handleMessage :: (HasApp m, MonadUnliftIO m) => MessageEvent -> TeamId -> m ()
handleMessage ev teamId = do
  workspace <- workspaceByTeamId teamId
  case ev.channelType of
    Channel -> do
      let links = mconcat $ extractLinks <$> fromMaybe [] ev.blocks
      repliedThreadIds <- mapMaybeM (handleUrl $ entityKey workspace) links
      -- this is like a n+1 query of STM, which is maybe bad for perf vs running
      -- it one action, but whatever
      forM_ repliedThreadIds $ \todo -> do
        for_ todo $ senderEnqueue . UpdateReply
    Im -> do
      handleImCommand (workspaceMetaFromWorkspaceE workspace) ev.channel ev.text ev.files
    Group ->
      -- we don't do these
      pure ()
  where
    handleUrl workspaceId url = do
      let linkSource =
            SlackUrlParts
              { channelId = ev.channel
              , messageTs = ev.ts
              , threadTs = ev.threadTs
              }
      for (splitSlackUrl url) \linkDestination -> do
        -- FIXME(evanr): The only IO these `record*` functions perform
        -- currently is database inserts, so I think they can/should be run in
        -- the same transaction.
        userId <- recordUser workspaceId ev.user
        recordLink workspaceId userId linkSource linkDestination

parseBotMessageAsUserMessage :: Value -> Maybe MessageEvent
parseBotMessageAsUserMessage v = case fromJSON (copyField "app_id" "user" v) of
  Success result -> Just result
  Error _        -> Nothing
  where 
    -- FIXME: add a BotMessageEvent type to the slack-web library
    -- a bot message won't have a "user" field, lets populate it from the "app_id" field
    copyField :: Text -> Text -> Value -> Value
    copyField srcField destField = \case
      Object obj -> case KeyMap.lookup (Key.fromText srcField) obj of
        Just val -> Object (KeyMap.insert (Key.fromText destField) val obj)
        Nothing -> Object obj -- Do nothing if the source field does not exist
      otherJson -> otherJson -- Return unchanged if it's not an Object

handleCallback :: Event -> TeamId -> Span -> AppM Value
handleCallback (EventMessage ev) teamId span | isNothing ev.botId = do
  blocklist <- getsApp (.config.blockedAppIds)
  case ev.appId of
    -- its unclear why sometimes slack sends the event with subtype bot_message
    -- and sometimes without it. in case slack's behavior changes in the future,
    -- we really want to prevent infinite loops, so we check the blocklist here
    -- as well
    Just appId | appId `elem` blocklist -> pure $ Object mempty
    _ -> do
      addAttribute span "slack.conversation.id" ev.channel.unConversationId
      addAttribute span "slack.event.appId" (fromMaybe "" ev.appId)
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
handleCallback (EventUnknown v) teamId span = do
  blocklist <- getsApp (.config.blockedAppIds)
  case parse typeAndSubtypeAndAppId v of
    Success ("message", Just "bot_message", Just appId) | appId `notElem` blocklist -> do
      case parseBotMessageAsUserMessage v of
        Just ev -> do
          addAttribute span "slack.conversation.id" ev.channel.unConversationId
          addAttribute span "slack.event.appId" appId
          handleMessage ev teamId
        Nothing -> logDebug $ "failed to parse bot message: " <> tshow v
    Success (type_, subtype, appId) -> do
      addAttribute span "slack.event.type" type_
      addAttribute span "slack.event.subtype" (fromMaybe "" subtype)
      addAttribute span "slack.event.appId" (fromMaybe "" appId)
      logUnknown
    _ -> logUnknown
  pure $ Object mempty
  where
    logUnknown = logDebug $ "unknown webhook callback: " <> tshow v

    typeAndSubtypeAndAppId :: Value -> Parser (Text, Maybe Text, Maybe Text)
    typeAndSubtypeAndAppId = withObject "webhook event" \val -> do
      type_ <- val .: "type"
      subtype <- val .:? "subtype"
      appId <- val .:? "app_id"
      pure (type_, subtype, appId)

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
      case err of
        -- This variant appears *after* the message has been cryptographically
        -- verified, so any appearance of this is probably a slack-web FromJSON
        -- bug.
        VerificationCannotParse _msg ->
          withCurrentSpan \s -> addAttribute s "payload" (decodeUtf8 body)
        _ -> pure ()
      throwIO $ VerificationException err
    Right todo -> do
      logDebug $ "payload: " <> cs body <> "\n\n"
      logDebug $ "webhook todo: " <> tshow todo <> "\n\n"
      inSpan (cs $ conNameOf todo) defaultSpanArguments $ do
        handleEvent todo
