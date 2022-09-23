module Slacklinker.Migrate where
import Slacklinker.Import
import Slacklinker.Persist
import Database.Persist.TH (migrateModels)
import Database.Persist.Postgresql (Migration, runSqlCommand, rawExecute)

-- FIXME(jadel): this lot should really use a migration runner, but i haven't
-- built that yet. w/e
migrateBase :: Migration
migrateBase = migrateModels allEntityDefs

migrateAll :: Migration
migrateAll = do
  runSqlCommand $ rawExecute "CREATE EXTENSION IF NOT EXISTS \"uuid-ossp\";" []
  migrateBase
