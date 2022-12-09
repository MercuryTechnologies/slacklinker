module Slacklinker.Tracing (
  inSpan,
  inSpan',
  OTel.defaultSpanArguments,
  withGlobalTracing,
  withCurrentSpan,
) where

import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Trace qualified as OTel
import Slacklinker.Prelude hiding (traceId)

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
