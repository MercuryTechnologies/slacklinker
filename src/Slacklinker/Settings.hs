module Slacklinker.Settings
  ( module Slacklinker.Settings.Types,
    module Slacklinker.Settings,
  )
where

import Database.Persist (PersistUniqueWrite (upsertBy))
import Database.Persist.Sql ((=.))
import Slacklinker.Import
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
