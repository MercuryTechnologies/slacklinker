module Slacklinker.Linear.SenderSpec (spec) where

import Data.GraphQL.Error (GraphQLError (..))
import OpenTelemetry.Attributes qualified as Attr
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Processor.Span (ShutdownResult (..), SpanProcessor (..))
import OpenTelemetry.Trace.Core qualified as OTel
import OpenTelemetry.Trace.Sampler (alwaysOn)
import Slacklinker.Linear.Sender (LinkResult (..), runBacklinkBatch)
import TestImport
import Web.Slack.Common (ResponseSlackError (..), SlackClientError (..))
import Web.Slack.Types (Emoji)

spec :: Spec
spec = describe "Linear backlink batch telemetry" do
  it "retains mixed results while still reacting as created" do
    withSpan \span -> do
      reactions <- runResults span (Created :| [badTicket, Duplicate]) (Right ())
      reactions `shouldBe` ["slacklinker-linear"]
      check span "partial_success" True [3, 3, 1, 1, 1] ["graphql_error"] [1, 0] []

  it "records explicit zeros and no reactions for all duplicates" do
    withSpan \span -> do
      reactions <- runResults span (Duplicate :| [Duplicate]) (Right ())
      reactions `shouldBe` []
      check span "all_duplicates" True [2, 2, 0, 2, 0] [] [0, 0] []

  it "treats created links plus duplicates as success" do
    withSpan \span -> do
      _ <- runResults span (Duplicate :| [Created]) (Right ())
      check span "success" True [2, 2, 1, 1, 0] [] [1, 0] []

  it "does not turn a successful backlink into a failed one when reacting fails" do
    withSpan \span -> do
      reactions <- runResults span (Created :| []) slackFailure
      reactions `shouldBe` ["slacklinker-linear"]
      check span "success" True [1, 1, 1, 0, 0] [] [1, 1] ["slack_api_error"]

  it "attempts both error reactions even when the first fails" do
    withSpan \span -> do
      reactions <- runResults span (badTicket :| [Duplicate, badTicket, OtherError []]) slackFailure
      reactions `shouldBe` ["slacklinker", "question"]
      check span "failure" True [4, 4, 0, 1, 3] ["empty_result", "graphql_error"] [2, 2] ["slack_api_error"]

  it "preserves completed work and distinguishes unattempted candidates on exceptions" do
    withSpan \span -> do
      let linkTicket () ticket = if ticket == (2 :: Int) then throwIO TestFailure else pure (Created, Nothing @())
      runBacklinkBatch span (1 :| [2, 3]) (pure ()) linkTicket noSave noReact `shouldThrow` isTestFailure
      check span "partial_success" False [3, 2, 1, 0, 1] ["link_exception"] [0, 0] []

  it "does not classify an interrupted duplicate-only prefix as all duplicates" do
    withSpan \span -> do
      let linkTicket () ticket = if ticket == (2 :: Int) then throwIO TestFailure else pure (Duplicate, Nothing @())
      runBacklinkBatch span (1 :| [2, 3]) (pure ()) linkTicket noSave noReact `shouldThrow` isTestFailure
      check span "failure" False [3, 2, 0, 1, 1] ["link_exception"] [0, 0] []

  it "records known zeros when setup fails before the first ticket" do
    withSpan \span -> do
      runBacklinkBatch span (Created :| [Duplicate]) (throwIO TestFailure) linkResult noSave noReact `shouldThrow` isTestFailure
      check span "failure" False [2, 0, 0, 0, 0] ["setup_exception"] [0, 0] []

  it "retains the created backlink if local persistence subsequently fails" do
    withSpan \span -> do
      let save () () = throwIO TestFailure
      runBacklinkBatch span (Created :| [Duplicate]) (pure ()) linkResult save noReact `shouldThrow` isTestFailure
      check span "partial_success" False [2, 1, 1, 0, 0] ["persistence_exception"] [0, 0] []

  it "keeps thrown reaction failures separate and propagates the exception" do
    withSpan \span -> do
      runBacklinkBatch span (Created :| []) (pure ()) linkResult noSave (const $ throwIO TestFailure) `shouldThrow` isTestFailure
      check span "success" True [1, 1, 1, 0, 0] [] [1, 1] ["reaction_exception"]

  it "enriches the owning span even while another span is current" do
    withSpan \owner -> do
      withSpan \child -> do
        _ <- runResults owner (Created :| []) (Right ())
        attrs <- OTel.spanGetAttributes child
        Attr.lookupAttribute attrs "linear.backlink.outcome" `shouldBe` Nothing
      check owner "success" True [1, 1, 1, 0, 0] [] [1, 0] []

data TestFailure = TestFailure
  deriving stock (Show)
  deriving anyclass (Exception)

isTestFailure :: TestFailure -> Bool
isTestFailure TestFailure = True

badTicket :: LinkResult
badTicket = OtherError [GraphQLError "private response message" Nothing Nothing]

slackFailure :: Either SlackClientError ()
slackFailure = Left $ SlackError $ ResponseSlackError "private response message" mempty

linkResult :: () -> LinkResult -> IO (LinkResult, Maybe ())
linkResult () result = pure (result, case result of Created -> Just (); _ -> Nothing)

noSave :: () -> () -> IO ()
noSave () () = pure ()

noReact :: Emoji -> IO (Either SlackClientError ())
noReact _ = expectationFailure "unexpected reaction" >> pure (Right ())

runResults :: OTel.Span -> NonEmpty LinkResult -> Either SlackClientError () -> IO [Emoji]
runResults span results reactionResult = do
  reactions <- newIORef []
  runBacklinkBatch span results (pure ()) linkResult noSave \emoji -> do
    modifyIORef' reactions (<> [emoji])
    pure reactionResult
  readIORef reactions

withSpan :: (OTel.Span -> IO ()) -> IO ()
withSpan action = bracket Context.getContext (void . Context.attachContext) \_ -> do
  -- Sampling of the surrounding test runner must not drop these test spans.
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
      let tracer = OTel.makeTracer provider "linear-batch-test" OTel.tracerOptions
      span <- OTel.inSpan' tracer "sender" OTel.defaultSpanArguments \span -> action span >> pure span
      ended <- OTel.unsafeReadSpan span
      ended.spanEnd `shouldSatisfy` isJust
      Attr.getDropped ended.spanAttributes `shouldBe` 0
      exported <- readIORef finished
      map (Attr.getAttributeMap . OTel.spanAttributes) exported `shouldBe` [Attr.getAttributeMap ended.spanAttributes]

-- Assert both values and OTLP types, including empty arrays and zero counts.
check :: OTel.Span -> Text -> Bool -> [Int] -> [Text] -> [Int] -> [Text] -> Expectation
check span outcome completed ticketCounts errors reactionCounts reactionErrors = do
  attrs <- OTel.spanGetAttributes span
  let expected =
        [("outcome", Attr.toAttribute outcome), ("completed", Attr.toAttribute completed), ("error_categories", Attr.toAttribute errors)]
          <> zipWith (\name count -> ("tickets." <> name, Attr.toAttribute count)) ["candidates", "attempted", "created", "duplicate", "failed"] ticketCounts
          <> zipWith (\name count -> ("reactions." <> name, Attr.toAttribute count)) ["attempted", "failed"] reactionCounts
          <> [("reactions.error_categories", Attr.toAttribute reactionErrors)]
  for_ expected \(name, value) ->
    Attr.lookupAttribute attrs ("linear.backlink." <> name) `shouldBe` Just value
