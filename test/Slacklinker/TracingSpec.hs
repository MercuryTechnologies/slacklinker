module Slacklinker.TracingSpec (spec) where

import Data.ByteString.Builder (toLazyByteString)
import Data.Typeable (typeOf)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import OpenTelemetry.Attributes qualified as Attributes
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware')
import OpenTelemetry.Processor.Span
import OpenTelemetry.Trace.Core qualified as OTel
import OpenTelemetry.Util (appendOnlyBoundedCollectionValues)
import Slacklinker.Exceptions
import Slacklinker.Sender (senderHandler)
import Slacklinker.Sender.Types (Terminate (..))
import Slacklinker.Tracing (inSpanWithTracer)
import TestImport
import Web.Slack.Experimental.RequestVerification

-- Snapshot in onEnd, rather than reading a mutable span after the operation.
-- Late annotations cannot make these assertions pass.
captureSpans :: (OTel.TracerProvider -> IO a) -> IO (a, [OTel.ImmutableSpan])
-- The SDK drops children of a dropped parent even with an always-on sampler.
-- Isolate these traces from Hspec's ambient span and restore its context after.
captureSpans action = bracket Context.detachContext (void . maybe Context.detachContext Context.attachContext) $ \_ -> do
  ended <- newIORef []
  let processor =
        SpanProcessor
          { spanProcessorOnStart = \_ _ -> pure ()
          , spanProcessorOnEnd = \ref -> do
              snapshot <- readIORef ref
              atomicModifyIORef' ended $ \spans -> (snapshot : spans, ())
          , spanProcessorShutdown = async $ pure ShutdownSuccess
          , spanProcessorForceFlush = pure ()
          }
  result <-
    bracket
      (OTel.createTracerProvider [processor] OTel.emptyTracerProviderOptions)
      OTel.shutdownTracerProvider
      action
  spans <- reverse <$> readIORef ended
  pure (result, spans)

operation :: OTel.TracerProvider -> Maybe ErrorSite -> Text -> IO a -> IO a
operation provider site name action =
  inSpanWithTracer (OTel.makeTracer provider "test" OTel.tracerOptions) site name OTel.defaultSpanArguments $ const action

attribute :: Text -> OTel.ImmutableSpan -> Maybe Text
attribute key span = Attributes.lookupAttribute span.spanAttributes key >>= Attributes.fromAttribute

exceptionEvents :: OTel.ImmutableSpan -> [OTel.Event]
exceptionEvents span = filter ((== "exception") . OTel.eventName) $ toList $ appendOnlyBoundedCollectionValues span.spanEvents

assertFailure :: OTel.ImmutableSpan -> Text -> Maybe Text -> Expectation
assertFailure span kind site = do
  span.spanEnd `shouldSatisfy` isJust
  attribute "slacklinker.error.kind" span `shouldBe` Just kind
  attribute "slacklinker.error.site" span `shouldBe` site
  span.spanStatus `shouldSatisfy` \case OTel.Error _ -> True; _ -> False
  let [event] = exceptionEvents span
  Attributes.lookupAttribute event.eventAttributes "exception.message" `shouldSatisfy` isJust
  Attributes.lookupAttribute event.eventAttributes "exception.stacktrace" `shouldSatisfy` isJust
  length (exceptionEvents span) `shouldBe` 1

assertSuccess :: OTel.ImmutableSpan -> Expectation
assertSuccess span = do
  span.spanEnd `shouldSatisfy` isJust
  attribute "slacklinker.error.kind" span `shouldBe` Nothing
  attribute "slacklinker.error.reason" span `shouldBe` Nothing
  attribute "slacklinker.error.site" span `shouldBe` Nothing
  span.spanStatus `shouldBe` OTel.Unset
  length (exceptionEvents span) `shouldBe` 0

-- Exercise the real WAI SDK response callback, which ends the request span.
httpFailure :: SomeException -> IO ((Status, ResponseHeaders, LByteString), [OTel.ImmutableSpan])
httpFailure exception = captureSpans $ \provider -> do
  response <- newIORef Nothing
  let app _ _ = throwIO exception
      respond resp = do
        let (code, responseHeaders_, withBody) = responseToStream resp
        body <- newIORef mempty
        withBody $ \stream -> stream (\chunk -> modifyIORef' body (<> chunk)) (pure ())
        bytes <- toLazyByteString <$> readIORef body
        writeIORef response $ Just (code, responseHeaders_, bytes)
        pure ResponseReceived
  void $ (newOpenTelemetryWaiMiddleware' provider . errorMiddleware) app defaultRequest respond
  Just result <- readIORef response
  pure result

spec :: Spec
spec = describe "error telemetry lifecycle" do
  it "classifies the job before it ends and does not record again on the parent in senderHandler" do
    (_, spans) <- captureSpans $ \provider ->
      operation provider Nothing "parent"
        $ operation provider (Just SenderJob) "job" (throwIO BadNonce)
        `catch` senderHandler "senderThread"
    let [job, parent] = spans
    job.spanName `shouldBe` "job"
    assertFailure job "Slacklinker.Exceptions.BadNonce" $ Just "sender.job"
    assertSuccess parent

  it "preserves exception propagation and classifies both a failed child and its owner" do
    (_, spans) <- captureSpans $ \provider -> do
      result <-
        try @IO @SomeException
          $ operation provider (Just SenderJob) "job"
          $ operation provider Nothing "dependency"
          $ throwIO (LinearOAuth2Error "upstream failed")
      case result of
        Left e -> case fromException e of
          Just (LinearOAuth2Error message_) -> message_ `shouldBe` "upstream failed"
          _ -> expectationFailure "exception type changed"
        Right () -> expectationFailure "exception was swallowed"
    let [dependency, job] = spans
    assertFailure dependency "Slacklinker.Exceptions.LinearOAuth2Error" Nothing
    assertFailure job "Slacklinker.Exceptions.LinearOAuth2Error" $ Just "sender.job"

  it "classifies external exceptions by runtime type while keeping their details in the event" do
    (_, [span]) <- captureSpans $ \provider ->
      operation provider Nothing "unknown" (throwIO $ userError "unique details")
        `shouldThrow` anyIOException
    assertFailure span (exceptionTypeName $ typeOf $ userError "") Nothing
    attribute "slacklinker.error.reason" span `shouldBe` Nothing

  for_
    [ (VerificationMissingTimestamp, "VerificationMissingTimestamp")
    , (VerificationMalformedTimestamp "yesterday", "VerificationMalformedTimestamp")
    , (VerificationTimestampOutOfRange 123, "VerificationTimestampOutOfRange")
    , (VerificationMissingSignature, "VerificationMissingSignature")
    , (VerificationUnknownSignatureVersion "v99", "VerificationUnknownSignatureVersion")
    , (VerificationMalformedSignature "bad hex", "VerificationMalformedSignature")
    , (VerificationUndecodableSignature "private signature", "VerificationUndecodableSignature")
    , (VerificationSignatureMismatch, "VerificationSignatureMismatch")
    , (VerificationCannotParse "private payload", "VerificationCannotParse")
    ]
    $ \(reason, name) ->
      it ("records " <> unpack name <> " separately from its exception type, without payloads") do
        (_, [span]) <- httpFailure $ toException $ VerificationException reason
        assertFailure span "Slacklinker.Exceptions.VerificationException" $ Just "http.request"
        attribute "slacklinker.error.reason" span `shouldBe` Just name

  it "records the verification reason on both a failing operation and its owner" do
    (_, spans) <- captureSpans $ \provider ->
      operation
        provider
        (Just SenderJob)
        "job"
        (operation provider Nothing "verify" $ throwIO $ VerificationException VerificationSignatureMismatch)
        `shouldThrow` (\(VerificationException reason) -> reason == VerificationSignatureMismatch)
    let [verification, job] = spans
    assertFailure verification "Slacklinker.Exceptions.VerificationException" Nothing
    assertFailure job "Slacklinker.Exceptions.VerificationException" $ Just "sender.job"
    for_ spans $ \span -> attribute "slacklinker.error.reason" span `shouldBe` Just "VerificationSignatureMismatch"

  it "leaves successful operations unclassified" do
    (result, [span]) <- captureSpans $ \provider -> operation provider (Just SenderJob) "success" $ pure (42 :: Int)
    result `shouldBe` 42
    assertSuccess span

  it "rethrows redirects through nested spans without error status or exception events" do
    (_, spans) <- captureSpans $ \provider ->
      operation provider Nothing "outer" (operation provider Nothing "inner" $ throwIO $ Redirect302 "/next")
        `shouldThrow` (\(Redirect302 location_) -> location_ == "/next")
    length spans `shouldBe` 2
    mapM_ assertSuccess spans

  it "preserves sender termination propagation" do
    senderHandler "test" (toException Terminate) `shouldThrow` (\Terminate -> True)

  for_
    [ (toException RegistrationDisabled, "RegistrationDisabled")
    , (toException LinearDisabled, "LinearDisabled")
    , (toException LinearNotAuthenticated, "LinearNotAuthenticated")
    ]
    $ \(exception, body) -> do
      it ("propagates " <> show exception <> " through nested spans without recording a failure") do
        (result, spans) <- captureSpans $ \provider ->
          try @IO @SomeException
            $ operation provider Nothing "outer"
            $ operation provider (Just SenderJob) "job"
            $ throwIO exception
        case result of
          Left actual -> (message <$> fromException @ServiceException actual) `shouldBe` Just (cs body)
          Right () -> expectationFailure "exception was swallowed"
        length spans `shouldBe` 2
        mapM_ assertSuccess spans

      it ("preserves the HTTP response for " <> show exception <> " without recording a failure") do
        (response, [span]) <- httpFailure exception
        response `shouldBe` (status400, [], body)
        assertSuccess span

  for_
    [ (toException BadNonce, status400, "BadNonce", "Slacklinker.Exceptions.BadNonce")
    , (toException $ VerificationException VerificationSignatureMismatch, status401, "VerificationSignatureMismatch", "Slacklinker.Exceptions.VerificationException")
    , (toException $ userError "internal details", status500, "Internal server error", exceptionTypeName $ typeOf $ userError "")
    ]
    $ \(exception, code, body, kind) ->
      it ("classifies HTTP " <> show code <> " before responding, with unchanged response") do
        (response, [span]) <- httpFailure exception
        response `shouldBe` (code, [], body)
        assertFailure span kind $ Just "http.request"

  it "preserves redirect status, location and body without marking the request as failed" do
    (response, [span]) <- httpFailure $ toException $ Redirect302 "/next"
    response `shouldBe` (status302, [(hLocation, "/next")], "Redirect")
    assertSuccess span
