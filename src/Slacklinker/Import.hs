module Slacklinker.Import
  ( module Slacklinker.Prelude,
    taggedOptions,
    snakeCaseOptions,
    snakeCaseFormOptions,
    AesonDecodeError(..),
    decodeThrow,
    orThrow,
    module Slacklinker.Tracing,
  )
where

import Data.Aeson
import Data.Aeson qualified as A
import Slacklinker.Exceptions
import Slacklinker.Prelude
import Slacklinker.Tracing (inSpan, defaultSpanArguments)
import Web.FormUrlEncoded (FormOptions, defaultFormOptions)
import Web.FormUrlEncoded qualified as F

snakeCaseOptions :: A.Options
snakeCaseOptions =
  defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '_'
    , A.constructorTagModifier = A.camelTo2 '_'
    }

snakeCaseFormOptions :: FormOptions
snakeCaseFormOptions =
  defaultFormOptions
    { F.fieldLabelModifier = A.camelTo2 '_'
    }

taggedOptions :: A.Options
taggedOptions = defaultOptions {A.sumEncoding = A.defaultTaggedObject}

decodeThrow :: (MonadIO m, FromJSON a) => ByteString -> m a
decodeThrow = fromEither . mapLeft AesonDecodeError . eitherDecodeStrict

orThrow :: (MonadIO m, Exception e) => Maybe a -> e -> m a
(Just a) `orThrow` _ = pure a
Nothing `orThrow` e = throwIO e
