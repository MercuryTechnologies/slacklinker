{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Settings.Types where

import Data.Aeson
import Data.Aeson.Types (parseEither)
import Slacklinker.Import
import Slacklinker.PersistImport

data SlacklinkerSetting
  = SettingAllowRegistration Bool
  | SettingRequireMutualTLS Bool

data SlacklinkerSettingTag = AllowRegistration | RequireMutualTLS
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Read)

$(derivePostgresEnumForSumType ''SlacklinkerSettingTag "slacklinker_setting_tag")

marshalSetting :: SlacklinkerSetting -> (SlacklinkerSettingTag, Value)
marshalSetting setting = case setting of
  SettingAllowRegistration v -> (AllowRegistration, toJSON v)
  SettingRequireMutualTLS v -> (RequireMutualTLS, toJSON v)

unmarshalSetting :: SlacklinkerSettingTag -> Value -> Either String SlacklinkerSetting
unmarshalSetting tag value = flip parseEither value $ case tag of
  AllowRegistration -> do
    withBool "AllowRegistration" \b -> pure $ SettingAllowRegistration b
  RequireMutualTLS -> do
    withBool "RequireMutualTLS" \b -> pure $ SettingRequireMutualTLS b
