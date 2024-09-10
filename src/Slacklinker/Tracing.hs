module Slacklinker.Tracing (
  inSpan,
  inSpan',
  OTel.defaultSpanArguments,
  withGlobalTracing,
  withCurrentSpan,
  flattenJsonToAttributes
) where

import OpenTelemetry.Attributes
import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Trace qualified as OTel
import Slacklinker.Prelude hiding (traceId)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Scientific qualified as Scientific
import Data.Vector qualified as Vector

getTracer :: MonadIO m => m OTel.Tracer
getTracer = do
  tp <- OTel.getGlobalTracerProvider
  pure $ OTel.makeTracer tp "slacklinker" OTel.tracerOptions

inSpan :: MonadUnliftIO m => Text -> OTel.SpanArguments -> m b -> m b
inSpan name args act = do
  tracer <- getTracer
  OTel.inSpan tracer name args act

inSpan' :: MonadUnliftIO m => Text -> OTel.SpanArguments -> (OTel.Span -> m b) -> m b
inSpan' name args act = do
  tracer <- getTracer
  OTel.inSpan' tracer name args act

withCurrentSpan :: MonadIO m => (OTel.Span -> m ()) -> m ()
withCurrentSpan act = do
  ctx <- OTel.lookupSpan <$> Context.getContext
  for_ ctx act

withGlobalTracing :: MonadUnliftIO m => m () -> m ()
withGlobalTracing act =
  bracket
    (liftIO OTel.initializeGlobalTracerProvider)
    (liftIO . OTel.shutdownTracerProvider)
    (const act)

-- | Flatten a JSON value into one or more attributes.
-- Dot-separated keys are used to represent nested objects.
-- Array indices are represented as [n].
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