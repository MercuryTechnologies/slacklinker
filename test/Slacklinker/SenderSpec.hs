module Slacklinker.SenderSpec (spec) where

import Data.Either (isLeft, isRight)
import Database.Persist
import Database.Persist.Sql (rawExecute)
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Types (status200)
import Network.Wai qualified as Wai
import Network.Wai.Handler.Warp (testWithApplication)
import OpenTelemetry.Attributes qualified as Attr
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Processor.Span (ShutdownResult (..), SpanProcessor (..))
import OpenTelemetry.Trace.Core qualified as OTel
import OpenTelemetry.Trace.Sampler (alwaysOn)
import OpenTelemetry.Util (appendOnlyBoundedCollectionSize)
import Slacklinker.App (App (..), runAppM, runDB)
import Slacklinker.Models
import Slacklinker.Sender (doUpdateReply)
import Test.HUnit.Lang (HUnitFailure)
import TestApp (withApp)
import TestImport
import TestUtils (createWorkspace)
import Web.PathPieces (toPathPiece)
import Web.Slack.Types (ConversationId (..))

data Result = Success | SlackFailure | PersistenceFailure
  deriving stock (Eq, Show)

spec :: Spec
spec = withApp $ describe "UpdateReply telemetry" do
  for_ [("create", Nothing, "/api/chat.postMessage"), ("update", Just "1700000001.000001", "/api/chat.update")] \(action, previousTs, endpoint) -> do
    for_ [Success, SlackFailure, PersistenceFailure] \result ->
      it (unpack action <> ": " <> show result) \app -> do
        (replyId, linkIds) <- fixture app previousTs
        when (result == PersistenceFailure) $ runAppM app $ runDB do
          -- Fail the second write, so success cannot mean merely saving replyTs.
          rawExecute "CREATE FUNCTION reject_sent_update() RETURNS trigger LANGUAGE plpgsql AS $$ BEGIN RAISE EXCEPTION 'test persistence failure'; END $$" []
          rawExecute "CREATE TRIGGER reject_sent_update BEFORE UPDATE ON linked_messages FOR EACH ROW EXECUTE FUNCTION reject_sent_update()" []

        withRecordedSpan \tracer owner -> do
          requests <- newIORef []
          let slackStub request respond = do
                modifyIORef' requests (<> [Wai.rawPathInfo request])
                checkPlan owner replyId action
                attribute owner "slack.reply.outcome" ("failure" :: Text)
                absent owner "slack.reply.ts"
                respond
                  $ Wai.responseLBS status200 [("Content-Type", "application/json")]
                  $ if result == SlackFailure
                    then "{\"ok\":false,\"error\":\"channel_not_found\"}"
                    else "{\"ok\":true,\"channel\":\"CDEST\",\"ts\":\"1700000002.000002\",\"text\":\"reply\",\"message\":{\"text\":\"reply\"}}"
          withSlack app slackStub \localApp ->
            -- Deliberately make another span current to catch accidental use of
            -- withCurrentSpan in the operation's enrichment.
            OTel.inSpan' tracer "dependency" OTel.defaultSpanArguments \child -> do
              doUpdateReplyResult <- try @IO @SomeException $ runAppM localApp $ doUpdateReply owner replyId
              absent child "slack.reply.outcome"
              checkPlan owner replyId action
              readIORef requests >>= (`shouldBe` [endpoint])
              (savedReply, savedLinks) <-
                runAppM app
                  $ runDB
                  $ (,)
                  <$> getJust replyId
                  <*> traverse getJust linkIds
              if result == Success
                then do
                  doUpdateReplyResult `shouldSatisfy` isRight
                  attribute owner "slack.reply.outcome" ("success" :: Text)
                  attribute owner "slack.reply.ts" ("1700000002.000002" :: Text)
                  savedReply.replyTs `shouldBe` Just "1700000002.000002"
                  map (.sent) savedLinks `shouldBe` [True, True]
                else do
                  doUpdateReplyResult `shouldSatisfy` isLeft
                  attribute owner "slack.reply.outcome" ("failure" :: Text)
                  if result == SlackFailure
                    then absent owner "slack.reply.ts"
                    else attribute owner "slack.reply.ts" ("1700000002.000002" :: Text)
                  savedReply.replyTs `shouldBe` previousTs
                  map (.sent) savedLinks `shouldBe` [False, True]
              -- Rethrow through the owning span to exercise SDK finalization.
              fromEither doUpdateReplyResult

  it "retains the internal ID and failure outcome when the initial read fails" \app -> do
    (replyId, linkIds) <- fixture app Nothing
    runAppM app $ runDB do
      deleteWhere [LinkedMessageId <-. linkIds]
      delete replyId
    withRecordedSpan \_ owner -> do
      result <- try @IO @SomeException $ runAppM app $ doUpdateReply owner replyId
      result `shouldSatisfy` isLeft
      attribute owner "slack.replied_thread.id" $ toPathPiece replyId
      attribute owner "slack.reply.outcome" ("failure" :: Text)
      for_ ["slack.team.id", "slack.channel", "slack.thread.ts", "slack.reply.action", "slack.reply.link_count", "slack.reply.ts"] $ absent owner
      fromEither result

fixture :: App -> Maybe Text -> IO (RepliedThreadId, [LinkedMessageId])
fixture app replyTs = runAppM app do
  (workspaceId, _) <- createWorkspace
  runDB do
    repliedThreadId <- insert RepliedThread {workspaceId, replyTs, conversationId = ConversationId "CDEST", threadTs = "1700000000.000001"}
    joinedChannelId <- insert JoinedChannel {workspaceId, channelId = ConversationId "CSOURCE", name = Just "source"}
    links <- for [("1700000000.000002", False), ("1700000000.000003", True)] \(messageTs, sent) ->
      insert LinkedMessage {repliedThreadId, joinedChannelId, knownUserId = Nothing, messageTs, threadTs = Nothing, sent}
    pure (repliedThreadId, links)

checkPlan :: OTel.Span -> RepliedThreadId -> Text -> Expectation
checkPlan owner replyId action = do
  attribute owner "slack.team.id" ("T0123" :: Text)
  attribute owner "slack.channel" ("CDEST" :: Text)
  attribute owner "slack.replied_thread.id" $ toPathPiece replyId
  attribute owner "slack.thread.ts" ("1700000000.000001" :: Text)
  attribute owner "slack.reply.action" action
  attribute owner "slack.reply.link_count" (2 :: Int)

attribute :: (Attr.ToAttribute a) => OTel.Span -> Text -> a -> Expectation
attribute span name value = do
  attrs <- OTel.spanGetAttributes span
  Attr.lookupAttribute attrs name `shouldBe` Just (Attr.toAttribute value)

absent :: OTel.Span -> Text -> Expectation
absent span name = do
  attrs <- OTel.spanGetAttributes span
  Attr.lookupAttribute attrs name `shouldBe` Nothing

withSlack :: App -> Wai.Application -> (App -> IO a) -> IO a
withSlack app application action = testWithApplication (pure application) \port -> do
  let settings = HTTP.defaultManagerSettings {HTTP.managerModifyRequest = \request -> pure request {HTTP.host = "127.0.0.1", HTTP.port = port, HTTP.secure = False}}
  manager <- HTTP.newManager settings
  action app {manager}

withRecordedSpan :: (OTel.Tracer -> OTel.Span -> IO ()) -> IO ()
withRecordedSpan action = bracket Context.getContext (void . Context.attachContext) \_ -> do
  void $ Context.attachContext Context.empty
  finished <- newIORef []
  let processor =
        SpanProcessor
          { spanProcessorOnStart = \_ _ -> pure ()
          , spanProcessorOnEnd = \ref -> readIORef ref >>= \s -> modifyIORef' finished (s :)
          , spanProcessorShutdown = async $ pure ShutdownSuccess
          , spanProcessorForceFlush = pure ()
          }
  bracket
    (OTel.createTracerProvider [processor] OTel.emptyTracerProviderOptions {OTel.tracerProviderOptionsSampler = alwaysOn})
    OTel.shutdownTracerProvider
    \provider -> do
      let tracer = OTel.makeTracer provider "update-reply-test" OTel.tracerOptions
      result <- try @IO @SomeException $ OTel.inSpan' tracer "UpdateReply" OTel.defaultSpanArguments $ action tracer
      exported <- filter ((== "UpdateReply") . OTel.spanName) <$> readIORef finished
      length exported `shouldBe` 1
      for_ exported \span -> do
        span.spanEnd `shouldSatisfy` isJust
        Attr.getDropped span.spanAttributes `shouldBe` 0
        let attrs = span.spanAttributes
        case result of
          Right () -> Attr.lookupAttribute attrs "slack.reply.outcome" `shouldBe` Just (Attr.toAttribute ("success" :: Text))
          Left err -> do
            -- Assertion failures must fail the test, not masquerade as expected
            -- operation exceptions.
            for_ (fromException err :: Maybe HUnitFailure) throwIO
            Attr.lookupAttribute attrs "slack.reply.outcome" `shouldBe` Just (Attr.toAttribute ("failure" :: Text))
            span.spanStatus `shouldSatisfy` \case OTel.Error _ -> True; _ -> False
            appendOnlyBoundedCollectionSize span.spanEvents `shouldBe` 1
