{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Slacklinker.Models where

import Data.Aeson (Value (..))
import Database.Persist (PersistField)
import Database.Persist.Postgresql (PersistFieldSql)
import Slacklinker.PersistImport
import Slacklinker.Settings.Types
import Slacklinker.Types
import Web.Slack.Types qualified as Slack

deriving newtype instance PersistField Slack.UserId
deriving newtype instance PersistFieldSql Slack.UserId

$(mkModelUnprefixed $(discoverEntities) $(modelFile "models"))
