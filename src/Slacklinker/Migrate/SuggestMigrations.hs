module Slacklinker.Migrate.SuggestMigrations (suggestMigrations) where

import Data.List qualified as List
import Data.Text.IO qualified as TIO
import Data.Time (nominalDiffTimeToSeconds)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Database.Persist.Sql (Sql, getMigration)
import Slacklinker.Import
import Slacklinker.Migrate
import System.Process.Typed (ExitCodeException (..), byteStringInput, proc, readProcessStdout_, setStdin)

formatMigration ::
  MonadUnliftIO m =>
  Text ->
  m Text
formatMigration migration = do
  let processConfig =
        setStdin (byteStringInput . encodeUtf8 . fromStrict $ migration) $
          proc "pg_format" ["-c", "db/pg_format.conf"]
  -- if the processing fails, just pass right through. it's fine.
  (decodeUtf8 . toStrict <$> readProcessStdout_ processConfig)
    `catch` (\ExitCodeException {} -> pure migration)

intentionalMismatches :: [Sql]
intentionalMismatches = []

suggestMigrations ::
  MonadUnliftIO m =>
  Text ->
  Bool ->
  SqlPersistT m ()
suggestMigrations migrationName dontFormat = do
  -- XXX: this sucks, but rust-refinery only supports 32 bit version numbers so
  -- I guess we get posix time in our filenames :(
  -- https://github.com/rust-db/refinery/pull/152
  versionNumber <-
    show @Int . truncate . nominalDiffTimeToSeconds . utcTimeToPOSIXSeconds
      <$> liftIO getCurrentTime

  let filename = "db/migrations" </> ("U" <> versionNumber <> "__" <> unpack migrationName <.> "sql")

  statements <- getMigration migrateBase
  let applicableMigrations = statements List.\\ intentionalMismatches

  let upSql_ = intercalate "\n" $ (<> ";") <$> applicableMigrations
  upSql <- if dontFormat then pure upSql_ else formatMigration upSql_

  liftIO $ do
    TIO.writeFile filename upSql

    putStrLn $ "Written: " <> pack filename
