{-# LANGUAGE QuasiQuotes #-}

module Slacklinker.Tracing (inSpan, OTel.defaultSpanArguments) where

import Slacklinker.Prelude hiding (traceId)
import qualified OpenTelemetry.Trace as OTel

inSpan :: MonadUnliftIO m => Text -> OTel.SpanArguments -> m b -> m b
inSpan name args act = do
  tp <- OTel.getGlobalTracerProvider
  let tracer = OTel.makeTracer tp "slacklinker" OTel.tracerOptions
  OTel.inSpan tracer name args act

