{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module Slacklinker.Models where

import Data.Aeson (Value (..))
import Slacklinker.PersistImport
import Slacklinker.Settings.Types
import Slacklinker.Types

$(mkModelUnprefixed $(discoverEntities) $(modelFile "models"))
