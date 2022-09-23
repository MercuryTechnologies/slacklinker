{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Slacklinker.Persist where

import Database.Persist (EntityDef)
import Slacklinker.Models ()
import Slacklinker.PersistImport

allEntityDefs :: [EntityDef]
allEntityDefs = $(discoverEntities)
