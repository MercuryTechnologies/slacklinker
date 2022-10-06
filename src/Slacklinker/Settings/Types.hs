{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Settings.Types where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Slacklinker.Import
import Slacklinker.PersistImport

data SlacklinkerSetting (a :: SlacklinkerSettingTag) where
  SettingAllowRegistration :: Bool -> SlacklinkerSetting 'AllowRegistration
  SettingRequireMutualTLS :: Bool -> SlacklinkerSetting 'RequireMutualTLS

data SlacklinkerSettingTag = AllowRegistration | RequireMutualTLS
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Read)

$(derivePostgresEnumForSumType ''SlacklinkerSettingTag "slacklinker_setting_tag")

-- | Existential to hide the tag of a SlacklinkerSetting in case it is runtime
-- determined.
data SlacklinkerSettingEx = forall a. MarshalledSetting a => SlacklinkerSettingEx (SlacklinkerSetting a)

-- | You don't want this function unless the tag is determined at runtime.
unmarshalSettingByTag :: SlacklinkerSettingTag -> Value -> Either String SlacklinkerSettingEx
unmarshalSettingByTag tag val = case tag of
  AllowRegistration -> SlacklinkerSettingEx <$> unmarshalSetting @'AllowRegistration val
  RequireMutualTLS -> SlacklinkerSettingEx <$> unmarshalSetting @'RequireMutualTLS val

{- | This is a funny DataKinds thing to let you decode settings as a GADT,
 which allows safely unwrapping the 'SlacklinkerSetting' by moving the
 tag parameter to a type parameter (so it is at compile time and GHC knows which
 variant it is)
-}
class MarshalledSetting (a :: SlacklinkerSettingTag) where
  -- | Turns the type into a value. Sorta like 'GHC.TypeNats.KnownNat'.
  reifyTag :: SlacklinkerSettingTag

  -- | Decodes the setting of this type-level tag.
  unmarshalSetting :: Value -> Either String (SlacklinkerSetting a)

  marshalSetting :: SlacklinkerSetting a -> Value

instance MarshalledSetting 'AllowRegistration where
  reifyTag = AllowRegistration

  marshalSetting (SettingAllowRegistration b) = toJSON b
  unmarshalSetting = parseEither $
    withBool "AllowRegistration" \b -> pure $ SettingAllowRegistration b

instance MarshalledSetting 'RequireMutualTLS where
  reifyTag = RequireMutualTLS

  marshalSetting (SettingRequireMutualTLS b) = toJSON b
  unmarshalSetting = parseEither $
    withBool "RequireMutualTLS" \b -> pure $ SettingRequireMutualTLS b
