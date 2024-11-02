{- | == Design of Slacklinker webhooks

   Slacklinker uses the [Slack Events API](https://api.slack.com/events) to
   get real time notifications of messages.

   Webhooks need to have their signature verified, which is done on the
   endpoint handler. After this, we know that the messages were sent by Slack
   and can be trusted.
-}
module Slacklinker.Handler.Webhook (postSlackInteractiveWebhookR, handleMessage) where

import Control.Monad.Extra (mapMaybeM)
import Data.Aeson (Result (..), Value (Object), (.:), (.:?), decodeStrict)
import Data.Aeson.Types (Parser, parse)
import Data.HashMap.Strict qualified as HashMap
import Database.Persist
import Generics.Deriving.ConNames (conNameOf)
import OpenTelemetry.Trace.Core (Span, addAttribute, addAttributes, ToAttribute (toAttribute), Attribute)
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
import Slacklinker.Extract.Types
import Slacklinker.Extract.FreeText (extractLinksFromJson)
import Data.List (nub)

extractBlockLinks :: SlackBlock -> [Text]
extractBlockLinks = fromBlock
  where
    fromBlock (SlackBlockRichText rt) = fromRichText rt
    fromBlock _ = []

    fromRichText rt = mconcat $ fromRichSectionItem <$> rt.elements
    fromRichSectionItem (RichTextSectionItemRichText rt) = mconcat $ fromRichItem <$> rt
    fromRichSectionItem _ = []

    fromRichItem (RichItemLink RichLinkAttrs {..}) = [url]
    fromRichItem _ = []

extractAttachedLinks :: DecodedMessageAttachment -> [Text]
extractAttachedLinks attachment = fromUrlLinks ++ blockLinks
  where
    fromUrlLinks :: [Text]
    fromUrlLinks = maybeToList attachment.fromUrl

    blockLinks :: [Text]
    blockLinks = maybe [] (concatMap extractBlockLinksFromMessageBlock) attachment.messageBlocks

    extractBlockLinksFromMessageBlock :: AttachmentMessageBlock -> [Text]
    extractBlockLinksFromMessageBlock messageBlock = concatMap extractBlockLinks messageBlock.message.blocks

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

handleMessage :: (HasApp m, MonadUnliftIO m, ExtractableMessage em) => em -> TeamId -> m ()
handleMessage msg teamId = do
  workspaceE@(Entity _ workspace) <- workspaceByTeamId teamId
  case ev.channelType of
    Channel -> do
      let blockLinks = mconcat $ extractBlockLinks <$> fromMaybe [] ev.blocks
          attachedLinks = mconcat $ extractAttachedLinks <$> mapMaybe decoded (fromMaybe [] ev.attachments)
          rawLinks = mconcat $ extractLinksFromJson workspace.slackSubdomain <$> maybe [] (map raw) ev.attachments
          links = nub $ blockLinks <> attachedLinks <> rawLinks
      repliedThreadIds <- mapMaybeM (handleUrl workspaceE) links
      -- this is like a n+1 query of STM, which is maybe bad for perf vs running
      -- it one action, but whatever
      forM_ repliedThreadIds $ \todo -> do
        for_ todo $ senderEnqueue . UpdateReply
    Im -> do
      handleImCommand (workspaceMetaFromWorkspaceE workspaceE) ev.channel ev.text ev.files
    Group ->
      -- we don't do these
      pure ()
  where
    ev = extractData msg
    handleUrl (Entity workspaceId workspace) url = do
      let linkSource =
            SlackUrlParts
              { workspaceName = workspace.slackSubdomain
              , channelId = ev.channel
              , messageTs = ev.ts
              , threadTs = ev.threadTs
              }
      for (splitSlackUrl url) \linkDestination -> do
        -- FIXME(evanr): The only IO these `record*` functions perform
        -- currently is database inserts, so I think they can/should be run in
        -- the same transaction.
        userId <- recordUser workspaceId ev.user
        recordLink workspaceId userId linkSource linkDestination

addEventAttributes :: Event -> TeamId -> Span -> AppM ()
addEventAttributes event teamId span = do
  addAttribute span "slack.team.id" teamId.unTeamId
  case event of
    EventMessage ev -> do
      addAttribute span "slack.event.type" ("message" :: Text)
      addAttribute span "slack.conversation.id" ev.channel.unConversationId
      addAttribute span "slack.event.appId" (fromMaybe "" ev.appId)
      addAttribute span "slack.event.botId" (fromMaybe "" ev.botId)
      addAttachmentAttributes ev.attachments span
    EventBotMessage ev -> do
      addAttribute span "slack.event.type" ("message" :: Text)
      addAttribute span "slack.event.subtype" ("bot_message" :: Text)
      addAttribute span "slack.conversation.id" ev.channel.unConversationId
      addAttribute span "slack.event.appId" (fromMaybe "" ev.appId)
      addAttribute span "slack.event.botId" ev.botId
      addAttachmentAttributes ev.attachments span
    EventMessageChanged -> do
      addAttribute span "slack.event.type" ("message" :: Text)
      addAttribute span "slack.event.subtype" ("message_changed" :: Text)
      pure ()
    EventChannelJoinMessage -> do
      addAttribute span "slack.event.type" ("message" :: Text)
      addAttribute span "slack.event.subtype" ("channel_join" :: Text)
      pure ()
    EventChannelCreated ev -> do
      addAttribute span "slack.event.type" ("channel_created" :: Text)
      addAttribute span "slack.channel.id" ev.channel.id.unConversationId
      addAttribute span "slack.channel.name" ev.channel.name
      addAttribute span "slack.channel.team.id" ev.channel.contextTeamId.unTeamId
    EventChannelLeft ev -> do
      addAttribute span "slack.event.type" ("channel_left" :: Text)
      addAttribute span "slack.channel.id" ev.channel.unConversationId
      addAttribute span "slack.channel.actor.id" ev.actorId.unUserId
    EventUnknown v -> case parse unknownAttributes v of
        Success attrs -> for_ attrs \(attrName, attrVal) -> addAttribute span ("slack.event." <> attrName) attrVal
        _ -> pure ()

  pure ()
  where
    addAttachmentAttributes :: Maybe [MessageAttachment] -> Span -> AppM ()
    addAttachmentAttributes mAttachments s = do
      addAttribute s "slack.event.hasAttachments" $ maybe False (not . null) mAttachments
      addAttribute s "slack.event.numAttachments" $ maybe 0 length mAttachments
      case mAttachments of
        Nothing -> pure ()
        Just attachments -> do
          addAttribute s "slack.event.attachments.hasMsgUnfurl" $ any hasMsgUnfurl attachments
          addAttribute s "slack.event.attachments.hasNoMessageBlocks" $ any hasNoMessageBlocks attachments
          addAttribute s "slack.event.attachments.hasUndecodable" $ any hasUndecodable attachments
      pure ()
    
    hasMsgUnfurl  :: MessageAttachment -> Bool
    hasMsgUnfurl = fromMaybe False . (decoded >=> isMsgUnfurl)

    -- decoded >=> (pure . isNothing . messageBlocks) - if decoding succeeds, checks if its messageBlocks field is Nothing
    -- fromMaybe True - if decoding fails, we treat it as having no message blocks (True)
    hasNoMessageBlocks :: MessageAttachment -> Bool
    hasNoMessageBlocks = fromMaybe True . (decoded >=> (pure . isNothing . messageBlocks))

    hasUndecodable :: MessageAttachment -> Bool
    hasUndecodable = isNothing . decoded

    filterMaybes :: [(a, Maybe b)] -> [(a, b)]
    filterMaybes = mapMaybe sequenceA

    -- adding these helps with debugging and adding new features to slacklinker
    unknownAttributes :: Value -> Parser [(Text, Attribute)]
    unknownAttributes = withObject "webhook event" \val -> do
      type_ <- val .: "type" :: Parser Text
      subtype <- val .:? "subtype" :: Parser (Maybe Text)
      appId <- val .:? "app_id" :: Parser (Maybe Text)
      botId <- val .:? "bot_id" :: Parser (Maybe Text)
      userId <- val .:? "user" :: Parser (Maybe Text)
      channelId <- val .:? "channel" :: Parser (Maybe Text)
      ts <- val .:? "ts" :: Parser (Maybe Text)
      team <- val .:? "team" :: Parser (Maybe Text)
      attachments <- val .:? "attachments" :: Parser (Maybe [Value])
      pure $ filterMaybes
        [ ("type", Just $ toAttribute type_)
        , ("subtype", toAttribute <$> subtype)
        , ("appId", toAttribute <$> appId)
        , ("botId", toAttribute <$> botId)
        , ("userId", toAttribute <$> userId)
        , ("channelId", toAttribute <$> channelId)
        , ("ts", toAttribute <$> ts)
        , ("team", toAttribute <$> team)
        , ("hasAttachments", Just . toAttribute . maybe False (not . null) $ attachments)
        , ("numAttachments", Just . toAttribute . maybe 0 length $ attachments)
        ]

handleCallback :: Event -> TeamId -> AppM ()
handleCallback event teamId = case event of
  -- Handle both regular and bot messages uniformly. 
  -- Some regular messages may actually be bot messages, maybe legacy bots.
  EventMessage ev -> handleMessage' ev 
  EventBotMessage ev -> handleMessage' ev
  
  -- Join newly created channels
  EventChannelCreated ev -> do
    Entity workspaceId workspace <- workspaceByTeamId teamId
    senderEnqueue $ JoinChannel 
      WorkspaceMeta
        { slackTeamId = workspace.slackTeamId
        , token = workspace.slackOauthToken
        , workspaceId
        }
      ev.channel.id
      
  -- If slacklinker was removed from a channel, remove the database entry
  EventChannelLeft ev -> do
    Entity wsId _ <- workspaceByTeamId teamId
    runDB $ deleteBy $ UniqueJoinedChannel wsId ev.channel
    
  -- No-op events
  EventMessageChanged -> pure ()
  EventChannelJoinMessage -> pure ()

  -- Log unknown events
  EventUnknown v -> logDebug $ "unknown webhook callback: " <> tshow v
  where
    handleMessage' :: (HasApp m, MonadUnliftIO m, ExtractableMessage em) => em -> m ()
    handleMessage' ev = do
      blocklist <- getsApp (.config.blockedAppIds)
      case (extractData ev).appId of
        Just appId | appId `elem` blocklist -> pure ()
        _ -> handleMessage ev teamId

handleEvent :: SlackWebhookEvent -> AppM Value
handleEvent (EventUrlVerification UrlVerificationPayload {..}) = do
  pure . toJSON $ UrlVerificationResponse {challenge}
handleEvent (EventEventCallback EventCallback {event, teamId}) = do
  inSpan' (cs $ conNameOf event) defaultSpanArguments \span -> do
    addEventAttributes event teamId span
    handleCallback event teamId
    pure $ Object mempty
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
        VerificationCannotParse _msg -> do
          -- waiting on a bugfix
          -- https://github.com/iand675/hs-opentelemetry/tree/attribute-length-limit-fix
          -- in the meantime, this will help us find issues
          let attrs = maybe [] (flattenJsonToAttributes "payload") (decodeStrict body)
          withCurrentSpan \s -> do 
            addAttribute s "payload" (decodeUtf8 body)
            addAttributes s (HashMap.fromList attrs)
        _ -> pure ()
      throwIO $ VerificationException err
    Right todo -> do
      logDebug $ "payload: " <> cs body <> "\n\n"
      logDebug $ "webhook todo: " <> tshow todo <> "\n\n"
      inSpan (cs $ conNameOf todo) defaultSpanArguments $ do
        handleEvent todo