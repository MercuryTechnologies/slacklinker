module Slacklinker.Handler.Webhook (postSlackInteractiveWebhookR) where

import Crypto.Hash (SHA256, digestFromByteString)
import Crypto.MAC.HMAC
import Data.Aeson (Value (Object))
import Data.ByteString.Base16 qualified as B16
import Data.ByteString.Char8 (readInt)
import Data.Either.Combinators (maybeToRight)
import Data.Text qualified as T
import Data.Time.Clock.POSIX
import Database.Persist
import Servant (ServerError, err400, err401, err500)
import Slacklinker.App
import Slacklinker.Exceptions
import Slacklinker.Handler.Webhook.Types
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender
import Slacklinker.SplitUrl
import Slacklinker.Types
import Web.Slack.Experimental.Blocks
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
  messageContent <- mMessageContent
  pure
    SendMessageReq
      { replyToTs = Just messageTs
      , channel = channelId
      , messageContent
      , workspaceMeta = workspaceMetaFromWorkspaceE wsE
      }
  where
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
helpMessage = unlines [
    "Welcome to Slacklinker. Here are the commands you can use:"
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
        Help -> senderEnqueue . SendMessage $ SendMessageReq {
            replyToTs = Nothing
            , channel = ev.channel
            , messageContent = helpMessage
            , workspaceMeta = workspaceMetaFromWorkspaceE workspace
          }
        JoinAll -> senderEnqueue $ ReqJoinAll (workspaceMetaFromWorkspaceE workspace) ev.channel

      pure ()
    Group ->
      -- we don't do these
      pure ()

handleCallback :: Event -> TeamId -> AppM Value
handleCallback (EventMessage ev) teamId | isNothing ev.botId = do
  handleMessage ev teamId
  pure $ Object mempty
-- if it's a bot message
handleCallback (EventMessage _ev) _ = pure $ Object mempty
handleCallback (EventMessageChanged) _ = pure $ Object mempty
handleCallback (EventChannelJoinMessage) _ = pure $ Object mempty
handleCallback (EventChannelCreated createdEvent) teamId = do
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
handleCallback (EventChannelLeft l) teamId = do
  -- remove our database entry stating we're in it
  Entity wsId _ <- workspaceByTeamId teamId
  runDB $ do
    deleteBy $ UniqueJoinedChannel wsId l.channel
  pure $ Object mempty
handleCallback (EventUnknown v) _ = do
  logInfo $ "unknown webhook callback: " <> tshow v
  pure $ Object mempty

handleEvent :: SlackWebhookEvent -> AppM Value
handleEvent (EventUrlVerification UrlVerificationPayload {..}) = do
  pure . toJSON $ UrlVerificationResponse {challenge}
handleEvent (EventEventCallback EventCallback {event, teamId}) = do
  handleCallback event teamId
handleEvent (EventUnknownWebhook v) = do
  logInfo $ "unknown webhook event: " <> tshow v
  pure $ Object mempty

postSlackInteractiveWebhookR :: SlackSignature -> SlackRequestTimestamp -> ByteString -> AppM Value
postSlackInteractiveWebhookR sig ts body = do
  ePayload <- validateRequest sig ts body
  case ePayload of
    Left err -> do
      logDebug $ "webhook err: " <> tshow err
      throwIO $ toStatus err
    Right payload -> do
      logDebug $ "payload: " <> cs payload <> "\n\n"
      todo <- decodeThrow payload
      logDebug $ "webhook todo: " <> tshow todo <> "\n\n"
      handleEvent todo

data SlackInteractiveException
  = SlackInteractiveMissingTimestamp
  | SlackInteractiveMalformedTimestamp ByteString
  | SlackInteractiveTimestampOutOfRange Int
  | SlackInteractiveMissingSignature
  | SlackInteractiveUnknownSignatureVersion ByteString
  | SlackInteractiveMalformedSignature String
  | SlackInteractiveUndecodableSignature ByteString
  | SlackInteractiveSignatureMismatch
  | SlackInteractiveCannotParse Text
  deriving stock (Show)

instance Exception SlackInteractiveException

toStatus :: SlackInteractiveException -> ServerError
toStatus SlackInteractiveSignatureMismatch = err401
toStatus (SlackInteractiveCannotParse _) = err500
toStatus _ = err400

-- See https://api.slack.com/authentication/verifying-requests-from-slack
validateRequest ::
  (HasApp m, MonadIO m) =>
  SlackSignature ->
  SlackRequestTimestamp ->
  ByteString ->
  m (Either SlackInteractiveException ByteString)
validateRequest (SlackSignature sigHeader) (SlackRequestTimestamp timestampString) body = do
  (SlackSigningSecret secret) <- getsApp (.config.slackSigningSecret)
  let fiveMinutes = 5 * 60
  now <- fmap utcTimeToPOSIXSeconds $ liftIO getCurrentTime
  pure $ do
    -- timestamp must be an Int for proper basestring construction below
    timestamp <-
      maybeToRight (SlackInteractiveMalformedTimestamp timestampString) $
        fst <$> readInt timestampString
    if abs (now - fromIntegral timestamp) > fiveMinutes
      then Left $ SlackInteractiveTimestampOutOfRange timestamp
      else Right ()
    sigHeaderStripped <-
      maybeToRight (SlackInteractiveUnknownSignatureVersion sigHeader) $
        stripPrefix "v0=" sigHeader
    sigDecoded <-
      mapLeft SlackInteractiveMalformedSignature $
        B16.decode sigHeaderStripped
    sig :: HMAC SHA256 <-
      maybeToRight (SlackInteractiveUndecodableSignature sigDecoded) $
        HMAC <$> digestFromByteString sigDecoded
    let basestring = encodeUtf8 ("v0:" <> tshow timestamp <> ":") <> body
    when (hmac secret basestring /= sig) $
      Left SlackInteractiveSignatureMismatch
    pure body
