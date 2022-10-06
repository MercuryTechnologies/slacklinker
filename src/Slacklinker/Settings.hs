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

setSetting :: MonadIO m => SlacklinkerSettingEx -> SqlPersistT m ()
setSetting setting = do
  let (tag, content) = marshalSettingEx setting
  void $
    upsertBy
      (UniqueSettingTag tag)
      (Setting {tag, content = JSONB content})
      [SettingContent =. JSONB content]

settingDefaultByTag :: SlacklinkerSettingTag -> Value
settingDefaultByTag AllowRegistration = toJSON True
settingDefaultByTag RequireMutualTLS = toJSON False

getSetting ::
  forall (tag :: SlacklinkerSettingTag) m.
  (MarshalledSetting tag, MonadIO m) =>
  SqlPersistT m (SlacklinkerSetting tag)
getSetting = do
  mSettingE <- getBy (UniqueSettingTag $ reifyTag @tag)
  let settingVal =
        maybe
          (settingDefaultByTag $ reifyTag @tag)
          (unJSONB . (.content) . entityVal)
          mSettingE
  fromEither . mapLeft AesonDecodeError $ unmarshalSetting @tag settingVal
