module Slacklinker.Handler.WebhookSpec (spec) where

import Control.Monad.Fail (fail)
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Data.Either (isLeft)
import Database.Persist
import Database.Persist.Sql (rawExecute)
import OpenTelemetry.Attributes qualified as Attr
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Processor.Span (ShutdownResult (..), SpanProcessor (..))
import OpenTelemetry.Trace.Core qualified as OTel
import OpenTelemetry.Trace.Id.Generator.Default (defaultIdGenerator)
import OpenTelemetry.Trace.Sampler (alwaysOn)
import Slacklinker.App (App (..), AppConfig (..), HasApp, runAppM, runDB)
import Slacklinker.Handler.TestData
import Slacklinker.Handler.TestUtils
import Slacklinker.Handler.Webhook (handleCallback, handleMessage)
import Slacklinker.Linear.WebhookSpec (goldenMessage)
import Slacklinker.Models
import Slacklinker.Sender.Types (SenderEnvelope (..), SenderRequest (..), senderChan)
import Slacklinker.SplitUrl (SlackUrlParts (..), splitSlackUrl)
import Test.HUnit.Lang (HUnitFailure)
import TestApp
import TestImport
import TestUtils (createWorkspace)
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Types

doLink :: (HasApp m, MonadUnliftIO m) => TeamId -> Text -> Text -> m MessageEvent
doLink teamId ts url = do
  let msg = messageEventWithBlocks ts [SlackBlockRichText . urlRichText $ url]
  handleMessage msg teamId
  pure msg

doBotLink :: (HasApp m, MonadUnliftIO m) => TeamId -> Text -> Text -> m BotMessageEvent
doBotLink teamId ts url = do
  let msg = botMessageEventWithBlocks ts [SlackBlockRichText . urlRichText $ url]
  handleMessage msg teamId
  pure msg

spec :: Spec
spec = do
  decisionSpec
  withApp $ describe "User insertion" do
    it "inserts a user when a link is sent" \app -> do
      runAppM app do
        (wsId, teamId) <- createWorkspace
        let (url, _parts) = sampleUrl
        msg <- doLink teamId ts1 url
        (Just (Entity _ userData)) <- runDB . getBy $ UniqueKnownUser wsId msg.user
        liftIO $ userData.emoji `shouldBe` Nothing

  withApp $ describe "Should insert RepliedThread for a message" do
    it "can deal with a simple link" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (url, parts) = sampleUrl
        msg <- doLink teamId ts1 url

        Just (Entity rtId _thread) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    it "can deal with a bot link" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (url, parts) = sampleUrl
        msg <- doBotLink teamId ts1 url

        (Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    forM_
      [ ("list", RichTextSectionItemList . pure . RichTextSection)
      , ("quote", RichTextSectionItemQuote)
      , ("preformatted block", RichTextSectionItemPreformatted)
      ]
      \(name, container) ->
        it ("can find a link in a rich-text " <> name) \app -> do
          runAppM app do
            (wsId, teamId) <- createWorkspace
            let (url, parts) = sampleUrl
                RichText {elements = [RichTextSectionItemRichText (RichTextSection items)]} = urlRichText url
                block = SlackBlockRichText $ RichText {blockId = Nothing, elements = [container items]}
                msg = messageEventWithBlocks ts1 [block]
            handleMessage msg teamId

            Just (Entity rtId _) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs
            links <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
            liftIO $ map ((.messageTs) . entityVal) links `shouldBe` [msg.ts]

    it "can deal with a forwarded url" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let msg = forwardedMessageEvent
            Just [MessageAttachment {decoded = Just DecodedMessageAttachment {fromUrl = Just url}}] = msg.attachments
            parts = fromJust $ splitSlackUrl url

        handleMessage msg teamId

        (Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    it "can deal with an attached url" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let msg = attachedUrlEvent
            Just [MessageAttachment {decoded = Just DecodedMessageAttachment {messageBlocks = Just [attachmentMessageBlock]}}] = msg.attachments
            AttachmentMessageBlock {message = AttachmentMessageBlockMessage {blocks = [SlackBlockRichText rt]}} = attachmentMessageBlock
            Just url = richTextToMaybeUrl rt
            parts = fromJust $ splitSlackUrl url

        handleMessage msg teamId

        (Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False
    it "can find URLs in undecodable attachments" \app -> do
      -- in this case, slacklinker will run the url detection parser on the raw json
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let msg = messageWithUndecodableAttachment
            Just [MessageAttachment {decoded = decodedAttachments}] = msg.attachments
        handleMessage msg teamId

        let expectedChannelId = ConversationId "C07KTH1T4CQ"
            expectedMessageTs = "1730339894.629249"

        (Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId expectedChannelId expectedMessageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- we check that decoding failed (if this fails, the test has been setup incorrectly)
          isNothing decodedAttachments `shouldBe` True
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    it "will not link a message within the same thread" \app -> do
      -- the setup should be
      -- lev: heres a parent message
      --  |-> (in thread) lev: i am linking to https://myworkspace.slack.com/archives/C045V0VJT16/p1725559477299859
      --
      -- where the parent message url is: https://myworkspace.slack.com/archives/C045V0VJT16/p1725559477299859
      -- and the thread message url is https://myworkspace.slack.com/archives/C045V0VJT16/p1725559485267619?thread_ts=1725559477.299859
      --
      -- importantly, parent.message_ts == child.thread_ts
      -- and parent.channel_id == child.channel_id
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (parentUrl, parentParts) = sampleUrl
            childMessage = messageEventWithBlocks ts1 [SlackBlockRichText . urlRichText $ parentUrl]
            childMessageWithThread = updateThreadTs childMessage (Just parentParts.messageTs)
            childMessageWithChannel = updateChannelId childMessageWithThread parentParts.channelId

        handleMessage childMessageWithChannel teamId

        -- We should not plan a reply to a thread that links to itself
        Nothing <- runDB $ getBy $ UniqueRepliedThread wsId parentParts.channelId parentParts.messageTs
        pure ()

    it "will file a link to a message downthread as the same thread as linking the parent" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        -- Create a message linking the thread parent

        let (childUrl, childUrlParts) = sampleUrlToChild
        void $ doLink teamId ts1 childUrl

        let (parentUrl, parentUrlParts) = sampleUrl
        void $ doLink teamId ts2 parentUrl

        -- Verify the test data reproduces the expected condition
        liftIO $ childUrlParts.threadTs `shouldBe` Just parentUrlParts.messageTs

        (Just _) <-
          runDB
            $ getBy
            $ UniqueRepliedThread
              wsId
              childUrlParts.channelId
              (fromJust childUrlParts.threadTs)

        allThreads <- runDB $ selectList @RepliedThread [] []
        liftIO $ length allThreads `shouldBe` 1
        pure ()

  withApp $ describe "Rich message mentions" do
    forM_ ["rich_text_section", "rich_text_list", "rich_text_quote", "rich_text_preformatted"] \container ->
      forM_ [False, True] \inAttachment ->
        it ("backlinks a URL-less mention in " <> unpack container <> if inAttachment then " inside an attachment" else "") \app -> do
          let (_, parts) = sampleUrl
              block = richTextBlock container [mention parts Nothing]
          msg <-
            if inAttachment
              then decodeMessage [] [attachmentWithBlock block]
              else decodeMessage [block] []
          runAppM app do
            (wsId, teamId) <- createWorkspace
            handleMessage msg teamId
            Just (Entity rtId _) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs
            [Entity _ linkedMessage] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
            Just (Entity channelId _) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel
            liftIO do
              linkedMessage.joinedChannelId `shouldBe` channelId
              linkedMessage.messageTs `shouldBe` msg.ts
              linkedMessage.threadTs `shouldBe` Nothing
              linkedMessage.sent `shouldBe` False

    forM_ [False, True] \inAttachment ->
      it ("keeps mention thread metadata when its URL omits it" <> if inAttachment then " in an attachment" else "") \app -> do
        let (_, parts) = sampleUrlToChild
            urlWithoutThread = "https://jadeapptesting.slack.com/archives/C045V0VJT16/p1668735634647249"
            block = richTextBlock "rich_text_section" [mention parts (Just urlWithoutThread)]
        msg <-
          if inAttachment
            then decodeMessage [] [attachmentWithBlock block]
            else decodeMessage [block] []
        runAppM app do
          (wsId, teamId) <- createWorkspace
          handleMessage msg teamId
          threads <- runDB $ selectList [RepliedThreadWorkspaceId ==. wsId] []
          liftIO $ map ((.threadTs) . entityVal) threads `shouldBe` [fromJust parts.threadTs]

    it "records one backlink when links and mentions target the same thread" \app -> do
      let (parentUrl, parentParts) = sampleUrl
          (childUrl, childParts) = sampleUrlToChild
          linkItem url = Aeson.object ["type" .= ("link" :: Text), "url" .= url]
          block = richTextBlock "rich_text_section" [mention childParts Nothing, linkItem childUrl, linkItem parentUrl]
      msg <- decodeMessage [block] [attachmentWithBlock $ richTextBlock "rich_text_quote" [mention childParts (Just childUrl)]]
      runAppM app do
        (wsId, teamId) <- createWorkspace
        handleMessage msg teamId
        [Entity rtId thread] <- runDB $ selectList [RepliedThreadWorkspaceId ==. wsId] []
        links <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        liftIO do
          thread.conversationId `shouldBe` parentParts.channelId
          thread.threadTs `shouldBe` parentParts.messageTs
          map ((.messageTs) . entityVal) links `shouldBe` [msg.ts]

    let (_, parentParts) = sampleUrl
        (_, childParts) = sampleUrlToChild
    forM_
      [ ("parent", parentParts, childParts.messageTs, Just parentParts.messageTs)
      , ("child", childParts, parentParts.messageTs, Nothing)
      , ("sibling", childParts, ts1, Just parentParts.messageTs)
      , ("message itself", parentParts, parentParts.messageTs, Nothing)
      ]
      \(name, destination, sourceTs, sourceThreadTs) ->
        it ("ignores a mention of a " <> name <> " in the same thread") \app -> do
          MessageEvent {..} <- decodeMessage [richTextBlock "rich_text_section" [mention destination Nothing]] []
          let msg = MessageEvent {channel = destination.channelId, ts = sourceTs, threadTs = sourceThreadTs, ..}
          runAppM app do
            (wsId, teamId) <- createWorkspace
            handleMessage msg teamId
            threads <- runDB $ selectList [RepliedThreadWorkspaceId ==. wsId] []
            liftIO $ length threads `shouldBe` 0

-- Decode the wire representation so tests cover both the dependency's parser
-- and extraction, including the separate attachment decoding path.
decodeMessage :: [Value] -> [Value] -> IO MessageEvent
decodeMessage blocks attachments =
  either fail pure
    $ parseEither Aeson.parseJSON
    $ Aeson.object
      [ "channel" .= ("C043YJGBY49" :: Text)
      , "channel_type" .= ("channel" :: Text)
      , "user" .= ("U043H11ES4V" :: Text)
      , "ts" .= ts1
      , "text" .= ("a message reference" :: Text)
      , "blocks" .= blocks
      , "attachments" .= attachments
      ]

mention :: SlackUrlParts -> Maybe Text -> Value
mention parts url =
  Aeson.object
    $ [ "type" .= ("message_mention" :: Text)
      , "channel_id" .= parts.channelId
      , "message_ts" .= parts.messageTs
      ]
    <> maybe [] (\ts -> ["thread_ts" .= ts]) parts.threadTs
    <> maybe [] (\u -> ["url" .= u]) url

richTextBlock :: Text -> [Value] -> Value
richTextBlock container items =
  Aeson.object
    [ "type" .= ("rich_text" :: Text)
    , "elements"
        .= [ Aeson.object
               [ "type" .= container
               , "elements"
                   .= if container == "rich_text_list"
                     then [Aeson.object ["type" .= ("rich_text_section" :: Text), "elements" .= items]]
                     else items
               ]
           ]
    ]

attachmentWithBlock :: Value -> Value
attachmentWithBlock block =
  Aeson.object
    [ "message_blocks"
        .= [ Aeson.object
               [ "team" .= ("T0123" :: Text)
               , "channel" .= ("C043YJGBY49" :: Text)
               , "ts" .= ts2
               , "message" .= Aeson.object ["blocks" .= [block]]
               ]
           ]
    ]

decisionSpec :: Spec
decisionSpec = withApp $ describe "Webhook decision telemetry" do
  it "processes callbacks without a tracing span" \app -> do
    (workspaceId, teamId) <- runAppM app createWorkspace
    let (url, parts) = sampleUrl
    runAppM app $ handleCallback Nothing (EventMessage $ messageEventWithBlocks ts1 [SlackBlockRichText $ urlRichText url]) teamId
    thread <- runAppM app $ runDB $ getBy $ UniqueRepliedThread workspaceId parts.channelId parts.messageTs
    thread `shouldSatisfy` isJust

  it "counts merged mention and URL references once while retaining thread metadata" \app -> do
    teamId <- decisionFixture app
    let (_, parts) = sampleUrlToChild
        url = "https://jadeapptesting.slack.com/archives/C045V0VJT16/p1668735634647249"
    msg <- decodeMessage [] [attachmentWithBlock $ richTextBlock "rich_text_section" [mention parts (Just url)]]
    withEventSpan "work_queued" \tracer owner -> do
      jobs <- processEvent tracer owner app (EventMessage msg) teamId
      jobs `shouldBe` ["update_reply"]
      checkCounts owner slackCountFields [2, 1, 1, 0, 1]
    threads <- runAppM app $ runDB $ selectList @RepliedThread [] []
    map ((.threadTs) . entityVal) threads `shouldBe` [fromJust parts.threadTs]

  it "ignores a blocked app before either integration is evaluated" \app -> do
    (_, teamId) <- runAppM app createWorkspace
    let blockedApp = app {config = app.config {blockedAppIds = ["XYZ123"]}}
        event = EventBotMessage $ botMessageEventWithBlocks ts1 [SlackBlockRichText $ urlRichText $ fst sampleUrl]
    withEventSpan "ignored" \tracer owner -> do
      jobs <- processEvent tracer owner blockedApp event teamId
      jobs `shouldBe` []
      attribute owner "slack.event.app_blocked" True
      attribute owner "slack.event.channel_type" ("channel" :: Text)
      for_ (slackCountFields <> linearCountFields <> ["linear.tickets.skip_reason"]) $ absent owner

  for_ [(Im, "im", "work_queued", ["send_message"]), (Group, "group", "ignored", [])] \(selectedChannelType, channelName, outcome, expectedJobs) ->
    it ("does not evaluate integrations for " <> unpack channelName) \app -> do
      (_, teamId) <- runAppM app createWorkspace
      let MessageEvent {..} = messageEventWithBlocks ts1 []
          event = EventMessage MessageEvent {channelType = selectedChannelType, ..}
      withEventSpan outcome \tracer owner -> do
        jobs <- processEvent tracer owner app event teamId
        jobs `shouldBe` expectedJobs
        attribute owner "slack.event.channel_type" (channelName :: Text)
        for_ (slackCountFields <> linearCountFields <> ["linear.tickets.skip_reason"]) $ absent owner

  let (parentUrl, parentParts) = sampleUrl
      (childUrl, _) = sampleUrlToChild
      message urls body = withMessageText (messageEventWithBlocks ts1 $ map (SlackBlockRichText . urlRichText) urls) body
      sameThread = updateChannelId (updateThreadTs (message [parentUrl] "hello") (Just parentParts.messageTs)) parentParts.channelId
  for_
    [ ("no candidates", message [] "hello", [0, 0, 0, 0, 0], [0, 0, 0, 0], "no_work", [])
    , ("unrecognized URL and unknown Linear team", message ["https://example.com"] "OTHER-1", [0, 0, 0, 0, 0], [1, 1, 0, 0], "no_work", [])
    , ("same-thread exclusion", sameThread, [1, 1, 1, 1, 0], [0, 0, 0, 0], "no_work", [])
    , ("Slack work despite no Linear candidates", updateThreadTs (message [parentUrl, parentUrl, childUrl] "hello") (Just ts2), [3, 2, 2, 0, 2], [0, 0, 0, 0], "work_queued", ["update_reply", "update_reply"])
    , ("Linear work despite no Slack candidates", message [] "ENG-1 ENG-1 ENG-2 OTHER-1", [0, 0, 0, 0, 0], [4, 3, 2, 1], "work_queued", ["linear:2"])
    , ("both integrations queue work", message [parentUrl] "ENG-1", [1, 1, 1, 0, 1], [1, 1, 1, 1], "work_queued", ["update_reply", "linear:1"])
    ]
    \(label, msg, slackCounts, linearCounts, outcome, expectedJobs) ->
      it label \app -> do
        teamId <- decisionFixture app
        withEventSpan outcome \tracer owner -> do
          jobs <- processEvent tracer owner app (EventMessage msg) teamId
          jobs `shouldBe` expectedJobs
          attribute owner "slack.event.app_blocked" False
          attribute owner "slack.event.channel_type" ("channel" :: Text)
          checkCounts owner slackCountFields slackCounts
          checkCounts owner linearCountFields linearCounts
          absent owner "linear.tickets.skip_reason"

  for_ [("linear", "linear_bot"), ("linear-new-ticket", "linear_app")] \(fixtureName, reason) ->
    it ("keeps Slack work when skipping " <> unpack reason) \app -> do
      teamId <- decisionFixture app
      EventEventCallback EventCallback {event} <- goldenMessage fixtureName
      -- Reuse real Linear webhook fixtures, adding a Slack link to prove that
      -- these exclusions apply only to Linear processing.
      let linkBlocks = Just [SlackBlockRichText $ urlRichText parentUrl]
          withLink = case event of
            EventMessage MessageEvent {..} -> EventMessage MessageEvent {blocks = linkBlocks, ..}
            EventBotMessage BotMessageEvent {..} -> EventBotMessage BotMessageEvent {blocks = linkBlocks, ..}
            _ -> error "expected message fixture"
      withEventSpan "work_queued" \tracer owner -> do
        jobs <- processEvent tracer owner app withLink teamId
        jobs `shouldBe` ["update_reply"]
        attribute owner "linear.tickets.skip_reason" (reason :: Text)
        attribute owner "slack.links.jobs_queued" (1 :: Int)
        for_ linearCountFields $ absent owner

  it "leaves unreached counts absent on workspace lookup failure" \app ->
    withEventSpan "failure" \_ owner -> do
      result <- try @IO @SomeException $ runAppM app $ handleCallback (Just owner) (EventMessage $ message [] "hello") (TeamId "UNKNOWN")
      result `shouldSatisfy` isLeft
      for_ (slackCountFields <> linearCountFields) $ absent owner
      fromEither result

  it "retains queued Slack work when the Linear team lookup fails" \app -> do
    teamId <- decisionFixture app
    runAppM app $ runDB $ rawExecute "ALTER TABLE linear_teams RENAME TO unavailable_linear_teams" []
    withEventSpan "failure" \_ owner -> do
      result <- try @IO @SomeException $ runAppM app $ handleCallback (Just owner) (EventMessage $ message [parentUrl] "ENG-1") teamId
      result `shouldSatisfy` isLeft
      checkCounts owner slackCountFields [1, 1, 1, 0, 1]
      attribute owner "linear.tickets.candidate_count" (1 :: Int)
      attribute owner "linear.tickets.unique_candidate_count" (1 :: Int)
      absent owner "linear.tickets.known_team_count"
      absent owner "linear.tickets.jobs_queued"
      fromEither result

withMessageText :: MessageEvent -> Text -> MessageEvent
withMessageText MessageEvent {..} body = MessageEvent {text = body, ..}

decisionFixture :: App -> IO TeamId
decisionFixture app = runAppM app do
  (workspaceId, teamId) <- createWorkspace
  runDB do
    organizationId <- insert $ LinearOrganization workspaceId "org" "org" "Test organization"
    insert_ $ LinearTeam organizationId "ENG"
  pure teamId

slackCountFields, linearCountFields :: [Text]
slackCountFields = map ("slack.links." <>) ["candidate_count", "unique_candidate_count", "recognized_count", "same_thread_excluded_count", "jobs_queued"]
linearCountFields = map ("linear.tickets." <>) ["candidate_count", "unique_candidate_count", "known_team_count", "jobs_queued"]

checkCounts :: OTel.Span -> [Text] -> [Int] -> Expectation
checkCounts owner names values = for_ (zip names values) \(name, value) -> attribute owner name value

attribute :: (Attr.ToAttribute a) => OTel.Span -> Text -> a -> Expectation
attribute span name value = do
  attrs <- OTel.spanGetAttributes span
  Attr.lookupAttribute attrs name `shouldBe` Just (Attr.toAttribute value)

absent :: OTel.Span -> Text -> Expectation
absent span name = do
  attrs <- OTel.spanGetAttributes span
  Attr.lookupAttribute attrs name `shouldBe` Nothing

-- Use a separate queue read end and filter by context so parallel tests cannot
-- consume or contaminate this event's observed jobs.
processEvent :: OTel.Tracer -> OTel.Span -> App -> Event -> TeamId -> IO [Text]
processEvent tracer owner app event teamId = do
  queue <- atomically $ dupTChan senderChan
  OTel.inSpan' tracer "dependency" OTel.defaultSpanArguments \child -> do
    runAppM app $ handleCallback (Just owner) event teamId
    for_ (slackCountFields <> linearCountFields <> ["slack.event.processing_outcome", "slack.event.app_blocked", "linear.tickets.skip_reason"]) $ absent child
    expectedContext <- OTel.getSpanContext child
    envelopes <- atomically $ drain queue
    matching <-
      filterM
        ( \envelope -> case Context.lookupSpan envelope.otelContext of
            Nothing -> pure False
            Just span -> (\spanContext -> spanContext.traceId == expectedContext.traceId) <$> OTel.getSpanContext span
        )
        envelopes
    pure $ map (jobName . (.request)) matching
  where
    drain queue =
      tryReadTChan queue >>= \case
        Nothing -> pure []
        Just envelope -> (envelope :) <$> drain queue
    jobName = \case
      UpdateReply _ -> "update_reply"
      SendMessage _ -> "send_message"
      BacklinkPlausibleLinearTickets _ _ _ _ tickets -> "linear:" <> tshow (length tickets)
      other -> "unexpected: " <> tshow other

withEventSpan :: Text -> (OTel.Tracer -> OTel.Span -> IO ()) -> IO ()
withEventSpan expectedOutcome action = bracket Context.getContext (void . Context.attachContext) \_ -> do
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
    (OTel.createTracerProvider [processor] OTel.emptyTracerProviderOptions {OTel.tracerProviderOptionsSampler = alwaysOn, OTel.tracerProviderOptionsIdGenerator = defaultIdGenerator})
    OTel.shutdownTracerProvider
    \provider -> do
      let tracer = OTel.makeTracer provider "webhook-test" OTel.tracerOptions
      result <- try @IO @SomeException $ OTel.inSpan' tracer "EventMessage" OTel.defaultSpanArguments $ action tracer
      case result of
        Left err -> do
          for_ (fromException err :: Maybe HUnitFailure) throwIO
          unless (expectedOutcome == "failure") $ throwIO err
        Right () -> expectedOutcome `shouldNotBe` "failure"
      exported <- filter ((== "EventMessage") . OTel.spanName) <$> readIORef finished
      length exported `shouldBe` 1
      for_ exported \span -> do
        span.spanEnd `shouldSatisfy` isJust
        Attr.getDropped span.spanAttributes `shouldBe` 0
        Attr.lookupAttribute span.spanAttributes "slack.event.processing_outcome" `shouldBe` Just (Attr.toAttribute expectedOutcome)
        when (expectedOutcome == "failure")
          $ span.spanStatus
          `shouldSatisfy` \case OTel.Error _ -> True; _ -> False
