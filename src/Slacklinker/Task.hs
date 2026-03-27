{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Slacklinker.Task where

import Data.Aeson (camelTo2)
import Data.Aeson qualified as A
import Database.Esqueleto.Experimental (Value (..))
import Options.Generic
import Slacklinker.App (App (..), AppConfig (..), AppM, HasApp (..), appShutdownNoSender, appStartupNoSender, makeApp, runAppM, runDB)
import Slacklinker.Exceptions (LinearDisabled (..))
import Slacklinker.Import
import Slacklinker.Linear.DB (linearAuthSessionsWithoutRefreshToken)
import Slacklinker.Linear.Session (migrateOldToken, updateAuthSession)
import Slacklinker.Linear.Teams (updateAllLinearTeamsCaches)
import Slacklinker.Migrate.SuggestMigrations (suggestMigrations)
import Slacklinker.Models (LinearAPIAuthSession (..))
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
  | UpdateAllLinearTeams
  | MigrateLinearTokens
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
  (MonadIO m) =>
  SlacklinkerSettingTag ->
  A.Value ->
  SqlPersistT m ()
doSetSetting settingName value = do
  setting <- fromEither . mapLeft AesonDecodeError $ unmarshalSettingByTag settingName value
  setSetting setting

runTask :: Task Unwrapped -> AppM ()
runTask (SuggestMigrations name dontFormat) = runDB $ suggestMigrations name dontFormat
runTask (SetSetting settingName value) = runDB $ doSetSetting settingName =<< decodeThrow value
runTask UpdateAllLinearTeams = updateAllLinearTeamsCaches
runTask MigrateLinearTokens = do
  linearCreds <- getsApp (.config.linearCreds) >>= (`orThrow` LinearDisabled)
  manager <- getsApp (.manager)
  sessions <- runDB linearAuthSessionsWithoutRefreshToken
  for_ sessions \(Value linearOrgId, Entity _ session) -> do
    logInfo $ "Migrating token for org " <> tshow linearOrgId
    tokenResponse <- liftIO $ migrateOldToken manager linearCreds session.token
    now <- liftIO getCurrentTime
    runDB $ updateAuthSession now linearOrgId tokenResponse
    logInfo $ "Migrated token for org " <> tshow linearOrgId
