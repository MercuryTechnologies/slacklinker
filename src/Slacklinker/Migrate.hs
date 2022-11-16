module Slacklinker.Migrate (migrateBase, migrateDatabase) where

import Data.ByteString.Builder (toLazyByteString)
import Data.HashMap.Strict qualified as HM
import Data.Monoid (Last (getLast))
import Database.Persist.Sql (Migration)
import Database.Persist.TH (migrateModels)
import Database.PostgreSQL.Simple.Options qualified as PG
import Safe (fromJustNote)
import Slacklinker.Persist
import Slacklinker.Prelude
import System.Environment (getEnvironment)
import System.Process.Typed
import URI.ByteString

migrateBase :: Migration
migrateBase = migrateModels allEntityDefs

{- | Serializes a 'PG.Options' into the right format for Refinery CLI, which is

 > postgres://user:pass@(urlencoded host)/db

 See <https://github.com/rust-db/refinery/blob/36e2c219de236c95853a0aa6cd606a2d52d255f8/refinery_core/src/config.rs#L225-L243>
-}
optionsToUri :: PG.Options -> Maybe (URIRef Absolute)
optionsToUri PG.Options {host, user, password, dbname, port} = do
  host' <- getLastBS host
  dbname' <- getLastBS dbname
  port' <- getLast port
  -- Postgres will be grumpy if you don't give it a user, so there's not any
  -- reasonable default here
  user' <- getLastBS user
  pure $
    URI
      { uriScheme = Scheme "postgres"
      , uriAuthority =
          Just $
            Authority
              { authorityUserInfo =
                  Just $ UserInfo user' (fromMaybe "" $ getLastBS password)
              , authorityHost = Host (cs . toLazyByteString $ urlEncode [] host')
              , authorityPort = Just $ Port port'
              }
      , uriPath = "/" <> cs dbname'
      , uriQuery = Query []
      , uriFragment = Nothing
      }
  where
    getLastBS :: Last String -> Maybe ByteString
    getLastBS s = cs <$> getLast s

-- | Migrates the database with the given connection string
migrateDatabase :: PG.Options -> IO ()
migrateDatabase opts = do
  env <- mapFromList <$> getEnvironment
  let postgresUri =
        cs
          . serializeURIRef'
          . fromJustNote "Conversion from PG.Options to URI failed"
          $ optionsToUri opts
  let newEnv =
        -- overrides items with the version in the first map
        HM.union
          (HM.singleton "POSTGRES_CONECTION_STRING" postgresUri)
          env
  -- Intentionally discard the output but record it so it gets into the
  -- exception but doesn't spam in the success case
  void $ readProcess_ $
    setEnv (HM.toList newEnv) $
      proc "refinery" ["migrate", "-e", "POSTGRES_CONECTION_STRING", "-p", "db/migrations"]
