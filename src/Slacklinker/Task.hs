{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Slacklinker.Task where

import Control.Exception qualified as Exception
import Data.Aeson (camelTo2)
import Data.Aeson qualified as A
import Database.Esqueleto.Experimental (Value (..))
import OpenTelemetry.Trace qualified as OTel
import Options.Generic
import Slacklinker.App (App (..), AppConfig (..), AppM, HasApp (..), appShutdownNoSender, appStartupNoSender, makeApp, runAppM, runDB)
import Slacklinker.Exceptions (LinearDisabled (..))
import Slacklinker.Import
import Slacklinker.Linear.DB (linearAuthSessionsWithoutRefreshToken)
import Slacklinker.Linear.Session (migrateOldToken, updateAuthSession)
import Slacklinker.Linear.Teams (linearTeamsCacheWorkspaces, updateLinearTeamsCache)
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

{- | Parse before initializing tracing, so help and parse errors need no provider.
The command span includes configuration, application startup, execution and
application teardown. It ends before provider shutdown flushes the summary.
-}
taskMain :: IO ()
taskMain = do
  record <- unwrapRecord "one-off-task"
  withGlobalTracing $ withTaskSpan record \owner -> do
    app_ <- makeApp
    bracket (appStartupNoSender app_) appShutdownNoSender $ \rti -> do
      let app = app_ {runtimeInfo = rti}
      runAppM app $ runTask owner record

{- | Own one parsed command's entire application lifetime. Only allowlisted
context is recorded. Exceptions are rethrown unchanged after the span ends:
letting the SDK record their messages could expose setting values, tokens or
connection strings. Mask finalization so cancellation also gets a summary.
-}
withTaskSpan :: Task Unwrapped -> (OTel.Span -> IO ()) -> IO ()
withTaskSpan task action = Exception.mask \restore -> do
  result <- inSpan' "maintenance.command" defaultSpanArguments \owner -> do
    addAttribute owner "maintenance.command.category" $ taskCategory task
    addAttribute owner "maintenance.command.outcome" ("failure" :: Text)
    case task of
      SuggestMigrations name _ -> addAttribute owner "maintenance.migration.name" name
      SetSetting name _ -> addAttribute owner "maintenance.setting.name" $ tshow name
      UpdateAllLinearTeams -> initializeOrganizationCounts owner
      MigrateLinearTokens -> initializeOrganizationCounts owner
    result <- Exception.try @SomeException $ restore $ action owner
    case result of
      Right () -> addAttribute owner "maintenance.command.outcome" ("success" :: Text)
      Left _ -> OTel.setStatus owner $ OTel.Error "Maintenance command failed"
    pure result
  either Exception.throwIO pure result

-- | Stable categories are independent of arguments, credentials and IDs.
taskCategory :: Task Unwrapped -> Text
taskCategory = \case
  SuggestMigrations {} -> "suggest-migrations"
  SetSetting {} -> "set-setting"
  UpdateAllLinearTeams -> "update-all-linear-teams"
  MigrateLinearTokens -> "migrate-linear-tokens"

{- | Attempts are known to be zero before setup; planned work is unknown until
the organization query succeeds, so its attribute is deliberately absent.
-}
initializeOrganizationCounts :: (MonadIO m) => OTel.Span -> m ()
initializeOrganizationCounts owner =
  for_ ["attempted", "completed", "failed"] \field ->
    addAttribute owner ("maintenance.organizations." <> field) (0 :: Int)

{- | Execute a materialized organization plan sequentially, stopping at the first
exception. An attempt begins just before its action; completion includes all of
that action's writes. Failed counts throwing attempts, including cancellation,
not unattempted organizations or failures in setup/teardown. Counts are written
to the owning span immediately so partial progress survives an exception.
-}
runOrganizations :: (MonadUnliftIO m) => OTel.Span -> [a] -> (a -> m ()) -> m ()
runOrganizations owner organizations action = do
  addAttribute owner "maintenance.organizations.planned" $ length organizations
  for_ (zip [1 :: Int ..] organizations) \(attempted, organization) ->
    mask \restore -> do
      addAttribute owner "maintenance.organizations.attempted" attempted
      restore (action organization)
        `onException` addAttribute owner "maintenance.organizations.failed" (1 :: Int)
      addAttribute owner "maintenance.organizations.completed" attempted

doSetSetting ::
  (MonadIO m) =>
  SlacklinkerSettingTag ->
  A.Value ->
  SqlPersistT m ()
doSetSetting settingName value = do
  setting <- fromEither . mapLeft AesonDecodeError $ unmarshalSettingByTag settingName value
  setSetting setting

runTask :: OTel.Span -> Task Unwrapped -> AppM ()
runTask _ (SuggestMigrations name dontFormat) = runDB $ suggestMigrations name dontFormat
runTask _ (SetSetting settingName value) = runDB $ doSetSetting settingName =<< decodeThrow value
runTask owner UpdateAllLinearTeams = do
  orgs <- linearTeamsCacheWorkspaces
  runOrganizations owner orgs updateLinearTeamsCache
runTask owner MigrateLinearTokens = do
  linearCreds <- getsApp (.config.linearCreds) >>= (`orThrow` LinearDisabled)
  manager <- getsApp (.manager)
  sessions <- runDB linearAuthSessionsWithoutRefreshToken
  runOrganizations owner sessions \(Value linearOrgId, Entity _ session) -> do
    logInfo $ "Migrating token for org " <> tshow linearOrgId
    tokenResponse <- liftIO $ migrateOldToken manager linearCreds session.token
    now <- liftIO getCurrentTime
    runDB $ updateAuthSession now linearOrgId tokenResponse
    logInfo $ "Migrated token for org " <> tshow linearOrgId
