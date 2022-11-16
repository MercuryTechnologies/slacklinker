module TestApp (pauseTest, withApp) where

import Control.Monad.Logger
import Database.PostgreSQL.Simple.Options qualified as PG
import Database.Postgres.Temp qualified as PGT
import Slacklinker.App
import Slacklinker.Migrate (migrateDatabase)
import Slacklinker.Prelude
import Test.Hspec

pauseTest :: (HasApp m, MonadIO m) => m ()
pauseTest = do
  connString <- getsApp (.config.postgresConnectionString)
  putStrLn $
    unlines
      [ "Test paused!"
      , "Connection string:"
      , cs connString
      , "Press any key to continue"
      ]
  void getChar

testAppConfig :: ByteString -> AppConfig
testAppConfig postgresConnectionString = AppConfig {slackClientSecret = credsErr, slackSigningSecret = credsErr, slackClientId = credsErr, postgresConnectionString, sqlLogLevel = LevelInfo, logLevel = LevelInfo}
  where
    credsErr = error "attempt to access credentials in test, this won't work"

withApp :: SpecWith App -> Spec
withApp = around \appAct ->
  -- FIXME(jadel): use 'withSnapshot' for speed; requires IO initialization
  -- surrounding all spec items, which I've not stared down yet
  fromEitherIO $ PGT.withConfig config $ \db -> do
    let opts = PGT.toConnectionOptions db
    migrateDatabase opts
    app_ <- makeApp' $ testAppConfig (PGT.toConnectionString db)

    bracket
      (appStartupNoSender app_)
      appShutdownNoSender
      \runtimeInfo -> appAct (app_ {runtimeInfo})
  where
    config = PGT.optionsToDefaultConfig (mempty {PG.user = pure "slacklinker"})
