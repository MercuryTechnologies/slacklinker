{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Sender.UserDataUpload.Types where

import Slacklinker.Prelude
import Slacklinker.Types (Emoji)

data UserData = UserData
  { callsign :: Maybe Emoji
  -- ^ Emoji callsign
  , email :: Text
  , gitHubUsername :: Maybe Text
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''UserData)

{- | Data about users to ingest. This is modeled to accept the data from the
 Mercury culture site's org chart response for now.
-}
newtype UserDataUpload = UserDataUpload
  { items :: Vector UserData
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''UserDataUpload)
