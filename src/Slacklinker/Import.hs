module Slacklinker.Import (
  module Slacklinker.Prelude,
  taggedOptions,
  snakeCaseOptions,
  screamingSnakeCaseOptions,
  dropConstructorPrefix,
  renameField,
  snakeCaseFormOptions,
  AesonDecodeError (..),
  decodeThrow,
  orThrow,
  module Slacklinker.Tracing,
) where

import Data.Aeson
import Data.Aeson qualified as A
import Slacklinker.Exceptions
import Slacklinker.Prelude
import Slacklinker.Tracing
import Web.FormUrlEncoded (FormOptions, defaultFormOptions)
import Web.FormUrlEncoded qualified as F

snakeCaseOptions :: A.Options
snakeCaseOptions =
  defaultOptions
    { A.fieldLabelModifier = A.camelTo2 '_'
    , A.constructorTagModifier = A.camelTo2 '_'
    }

screamingSnakeCaseOptions :: A.Options
screamingSnakeCaseOptions =
  defaultOptions
    { A.fieldLabelModifier = toUpper . A.camelTo2 '_'
    , A.constructorTagModifier = toUpper . A.camelTo2 '_'
    }

dropConstructorPrefix :: String -> A.Options -> A.Options
dropConstructorPrefix prefix opts = opts {A.constructorTagModifier = A.constructorTagModifier opts . strip}
  where
    strip tag = fromMaybe (error $ "dropConstructorPrefix: " <> tag <> " lacks prefix " <> prefix) (stripPrefix prefix tag)

renameField :: String -> String -> A.Options -> A.Options
renameField this that opts = opts {A.fieldLabelModifier = (\f -> if f == this then that else f) . A.fieldLabelModifier opts}

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
