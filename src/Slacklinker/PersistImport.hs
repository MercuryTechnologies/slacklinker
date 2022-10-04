{-# OPTIONS_GHC -Wno-orphans #-}

module Slacklinker.PersistImport
  ( module Database.Persist.TH,
    module Slacklinker.UUID,
    module Slacklinker.Import,
    module Web.Slack.Common,
    module Database.Esqueleto.PostgreSQL.JSON,
    module Slacklinker.Persist.DeriveEnum,
    mkModelUnprefixed,
    modelFile,
  )
where

import Database.Esqueleto.PostgreSQL.JSON
import Database.Persist
import Database.Persist.Quasi.Internal
import Database.Persist.Sql (PersistFieldSql)
import Database.Persist.TH
import Language.Haskell.TH
import Slacklinker.Import
import Slacklinker.Persist.DeriveEnum
import Slacklinker.UUID
import Web.Slack.Common (ConversationId (..), TeamId (..))

deriving newtype instance PersistField TeamId

deriving newtype instance PersistFieldSql TeamId

deriving newtype instance PersistFieldSql ConversationId

deriving newtype instance PersistField ConversationId

-- | Makes a model file with unprefixed field names.
mkModelUnprefixed :: [EntityDef] -> [UnboundEntityDef] -> Q [Dec]
mkModelUnprefixed =
  mkPersistWith unprefixedSettings
  where
    unprefixedSettings = sqlSettings {mpsFieldLabelModifier = \_modelName fieldName -> fieldName}

{- | Splice in the contents of the model file.

 Used like:

 @
 modelFile "foo"
 @

 which would load a field @config/modelsFiles/foo.persistentmodels@.
-}
modelFile :: String -> Q Exp
modelFile modelName =
  persistFileWith lowerCaseSettings (prefix <> modelName <> ".persistentmodels")
  where
    prefix =
      "config/modelsFiles/"
