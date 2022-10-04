module Slacklinker.Settings
  ( module Slacklinker.Settings.Types,
    module Slacklinker.Settings,
  )
where

import Data.Aeson (Value)
import Database.Persist (PersistUniqueWrite (upsertBy), getBy)
import Database.Persist.Sql ((=.))
import Slacklinker.Models
import Slacklinker.PersistImport
import Slacklinker.Settings.Types

setSetting :: MonadIO m => SlacklinkerSetting -> SqlPersistT m ()
setSetting setting = do
  let (tag, content) = marshalSetting setting
  void $
    upsertBy
      (UniqueSettingTag tag)
      (Setting {tag, content = JSONB content})
      [SettingContent =. JSONB content]

settingDefaultByTag :: SlacklinkerSettingTag -> Value
settingDefaultByTag AllowRegistration = toJSON True
settingDefaultByTag RequireMutualTLS = toJSON False

getSetting :: MonadIO m => SlacklinkerSettingTag -> SqlPersistT m SlacklinkerSetting
getSetting tag = do
  mSettingE <- getBy (UniqueSettingTag tag)
  let settingVal =
        maybe
          (settingDefaultByTag tag)
          (unJSONB . (.content) . entityVal)
          mSettingE
  fromEither . mapLeft AesonDecodeError $ unmarshalSetting tag settingVal
