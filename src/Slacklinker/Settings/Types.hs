{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Settings.Types where

import Slacklinker.Import
import Slacklinker.PersistImport

type role SlacklinkerSetting nominal
data SlacklinkerSetting (a :: SlacklinkerSettingTag) where
  -- | Whether to allow any access to register new workspaces.
  SettingAllowRegistration :: Bool -> SlacklinkerSetting 'AllowRegistration
  -- | Not implemented yet!
  SettingRequireMutualTLS :: Bool -> SlacklinkerSetting 'RequireMutualTLS
  -- | Whether to allow uploading user data such as user emoji.
  SettingAllowUploadUserData :: Bool -> SlacklinkerSetting 'AllowUploadUserData

data SlacklinkerSettingTag = AllowRegistration | RequireMutualTLS | AllowUploadUserData
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic, Read)

$(derivePostgresEnumForSumType ''SlacklinkerSettingTag "slacklinker_setting_tag")
