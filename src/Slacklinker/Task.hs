{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Slacklinker.Task where

import Data.Aeson (Value, camelTo2)
import Options.Generic
import Slacklinker.App (App (..), AppM, appShutdownNoSender, appStartupNoSender, makeApp, runAppM, runDB)
import Slacklinker.Import
import Slacklinker.Migrate.SuggestMigrations (suggestMigrations)
import Slacklinker.Settings

instance ParseFields SlacklinkerSettingTag

instance ParseField SlacklinkerSettingTag

instance ParseRecord SlacklinkerSettingTag

type role Task nominal
data Task w
  = SuggestMigrations
      { migrationName :: w ::: Text <?> "What to call the migration to generate"
      , dontFormat :: w ::: Bool <?> "Skip formatting the migration"
      }
  | SetSetting
      { settingName :: w ::: SlacklinkerSettingTag
      , value :: w ::: ByteString
      }
  deriving stock (Generic)

kebabCaseModifier :: Modifiers
kebabCaseModifier = defaultModifiers {constructorNameModifier = camelTo2 '-'}

instance ParseRecord (Task Wrapped) where
  parseRecord = parseRecordWithModifiers kebabCaseModifier

taskMain :: IO ()
taskMain = do
  record <- unwrapRecord "one-off-task"
  app_ <- makeApp
  bracket (appStartupNoSender app_) appShutdownNoSender $ \rti -> do
    let app = app_ {runtimeInfo = rti}
    runAppM app $ runTask record

doSetSetting ::
  MonadIO m =>
  SlacklinkerSettingTag ->
  Value ->
  SqlPersistT m ()
doSetSetting settingName value = do
  setting <- fromEither . mapLeft AesonDecodeError $ unmarshalSettingByTag settingName value
  setSetting setting

runTask :: Task Unwrapped -> AppM ()
runTask (SuggestMigrations name dontFormat) = runDB $ suggestMigrations name dontFormat
runTask (SetSetting settingName value) = runDB $ doSetSetting settingName =<< decodeThrow value
