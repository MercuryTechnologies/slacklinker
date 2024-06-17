{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}

module Slacklinker.Models where

import Data.Aeson (Value (..))
import Slacklinker.PersistImport
import Slacklinker.Settings.Types
import Slacklinker.Types
import Web.Slack.Types qualified as Slack

$(mkModelUnprefixed $(discoverEntities) $(modelFile "models"))
