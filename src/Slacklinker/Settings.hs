module Slacklinker.Settings
  ( module Slacklinker.Settings.Types,
    module Slacklinker.Settings,
  )
where

import Data.Aeson (Value, withBool)
import Database.Persist (PersistUniqueWrite (upsertBy), getBy)
import Database.Persist.Sql ((=.))
import Slacklinker.Models
import Slacklinker.PersistImport
import Slacklinker.Settings.Types
import Data.Aeson.Types (parseEither)

-- | Existential to hide the tag of a SlacklinkerSetting in case it is runtime
-- determined.
--
-- NOTE: this is kinda janky: it's dealing with the fact that we can't pass
-- around \"SlacklinkerSetting with any value of \'a\'\". It could possibly be
-- \"improved\" through the use of the \'singletons\' package, although it is
-- up to the reader whether that is an improvement or more cursed code.
--
-- It's basically only used in the case of the one-off-tasks and sadly requires
-- 'unmarshalSettingByTag' and 'marshalSettingEx' which are pure boilerplate.
data SlacklinkerSettingEx = forall a. MarshalledSetting a => SlacklinkerSettingEx (SlacklinkerSetting a)

-- | You don't want this function unless the tag is determined at runtime.
unmarshalSettingByTag :: SlacklinkerSettingTag -> Value -> Either String SlacklinkerSettingEx
unmarshalSettingByTag tag val = case tag of
  AllowRegistration -> SlacklinkerSettingEx <$> unmarshalSetting @'AllowRegistration val
  RequireMutualTLS -> SlacklinkerSettingEx <$> unmarshalSetting @'RequireMutualTLS val

-- | You don't want this function unless the tag is determined at runtime.
marshalSettingEx :: SlacklinkerSettingEx -> (SlacklinkerSettingTag, Value)
marshalSettingEx = \case
  SlacklinkerSettingEx (a@(SettingAllowRegistration _) :: SlacklinkerSetting tag) ->
    (reifyTag @tag, marshalSetting a)
  SlacklinkerSettingEx (a@(SettingRequireMutualTLS _) :: SlacklinkerSetting tag) ->
    (reifyTag @tag, marshalSetting a)

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

  -- | Marshals the setting to the JSON of its content.
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

-- | Example usage (the tilde is an irrefutable pattern, which is unambiguous
-- due to the GADT):
--
-- @
-- ~(SettingAllowRegistration r) <- runDB $ getSetting @'AllowRegistration
-- @
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
