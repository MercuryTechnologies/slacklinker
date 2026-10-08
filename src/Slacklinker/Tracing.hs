module Slacklinker.Tracing (
  inSpan,
  inSpan',
  inSpanAt,
  inSpanWithTracer,
  addAttribute,
  addAttributes,
  OTel.defaultSpanArguments,
  withGlobalTracing,
  withTracingProvider,
  withCurrentSpan,
  flattenJsonToAttributes,
) where

import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific qualified as Scientific
import Data.Vector qualified as Vector
import OpenTelemetry.Attributes (Attribute, ToAttribute (..))
import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Trace (addAttribute, addAttributes)
import OpenTelemetry.Trace qualified as OTel
import Slacklinker.Exceptions (ErrorSite, ExceptionClassification (..), annotateError, classifyException)
import Slacklinker.Prelude hiding (traceId)

getTracer :: (MonadIO m) => m OTel.Tracer
getTracer = do
  tp <- OTel.getGlobalTracerProvider
  pure $ OTel.makeTracer tp "slacklinker" OTel.tracerOptions

inSpan :: (MonadUnliftIO m) => Text -> OTel.SpanArguments -> m b -> m b
inSpan name args act = inSpan' name args $ const act

inSpan' :: (MonadUnliftIO m) => Text -> OTel.SpanArguments -> (OTel.Span -> m b) -> m b
inSpan' name args act = do
  tracer <- getTracer
  inSpanWithTracer tracer Nothing name args act

inSpanAt :: (MonadUnliftIO m) => ErrorSite -> Text -> OTel.SpanArguments -> (OTel.Span -> m b) -> m b
inSpanAt site name args act = do
  tracer <- getTracer
  inSpanWithTracer tracer (Just site) name args act

{- | Annotate before the SDK records the exception and ends the owning span.
Expected outcomes leave the SDK normally, then resume unwinding outside the span.
-}
inSpanWithTracer :: (MonadUnliftIO m) => OTel.Tracer -> Maybe ErrorSite -> Text -> OTel.SpanArguments -> (OTel.Span -> m b) -> m b
inSpanWithTracer tracer site name args act = do
  result <- OTel.inSpan' tracer name args $ \span ->
    (Right <$> act span) `catch` \e -> case classifyException e of
      ExpectedOutcome -> pure $ Left e
      Failed kind reason -> do
        annotateError span site kind reason
        throwIO e
  either throwIO pure result

withCurrentSpan :: (MonadIO m) => (OTel.Span -> m ()) -> m ()
withCurrentSpan act = do
  ctx <- OTel.lookupSpan <$> Context.getContext
  for_ ctx act

{- | Initialize process-wide tracing and flush/shut it down after the action,
including when the action throws. All owning spans must end inside the action.
-}
withGlobalTracing :: (MonadUnliftIO m) => m () -> m ()
withGlobalTracing = withTracingProvider (liftIO OTel.initializeGlobalTracerProvider)

{- | Bracket a provider's lifetime. Keeping acquisition explicit also permits
in-memory processors to exercise the same shutdown path as global tracing.
-}
withTracingProvider :: (MonadUnliftIO m) => m OTel.TracerProvider -> m () -> m ()
withTracingProvider acquire act =
  bracket
    acquire
    (liftIO . OTel.shutdownTracerProvider)
    (const act)

{- | Flatten a JSON value into one or more attributes.
Dot-separated keys are used to represent nested objects.
Array indices are represented as [n].
-}
flattenJsonToAttributes :: Text -> Aeson.Value -> [(Text, Attribute)]
flattenJsonToAttributes attributePrefix = \case
  Aeson.Object o ->
    let keysAndValues = KeyMap.toList o
        flattenObject (k, v) = flattenJsonToAttributes (attributePrefix <> "." <> Key.toText k) v
     in concatMap flattenObject keysAndValues
  Aeson.Array xs ->
    let indexedVector = Vector.indexed xs
        flattenVector (i, v) = flattenJsonToAttributes (attributePrefix <> "[" <> tshow i <> "]") v
     in concatMap flattenVector indexedVector
  Aeson.String x ->
    [(attributePrefix, toAttribute x)]
  Aeson.Number x ->
    case Scientific.floatingOrInteger x of
      Left d ->
        [(attributePrefix, toAttribute @Double d)]
      Right i ->
        [(attributePrefix, toAttribute @Int64 i)]
  Aeson.Bool x ->
    [(attributePrefix, toAttribute x)]
  Aeson.Null ->
    [(attributePrefix, toAttribute @Text "null")]
