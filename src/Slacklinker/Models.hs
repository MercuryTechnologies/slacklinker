{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Slacklinker.Models where

import Slacklinker.PersistImport
import Slacklinker.Types

$(mkModelUnprefixed $(discoverEntities) $(modelFile "models"))
