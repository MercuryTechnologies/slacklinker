{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Sender.UserDataUpload.Types where

import Data.Aeson ((.:), (.:?))
import Slacklinker.Prelude
import Slacklinker.Types (Emoji)

data UserData = UserData
  { emoji :: Maybe Emoji
  -- ^ Emoji callsign
  , email :: Text
  , gitHubUsername :: Maybe Text
  }
  deriving stock (Show)

instance FromJSON UserData where
  parseJSON = withObject "UserData" \o -> do
    -- for compatibility with the Mercury schema that calls the emoji
    -- "callsign".
    callsign <- o .:? "callsign"
    emoji_ <- o .:? "emoji"
    let emoji = callsign <|> emoji_
    gitHubUsername <- o .:? "gitHubUsername"
    email <- o .: "email"
    pure UserData {emoji, gitHubUsername, email}

{- | Data about users to ingest. This is modeled to accept the data from the
 Mercury culture site's org chart response for now.
-}
newtype UserDataUpload = UserDataUpload
  { items :: Vector UserData
  }
  deriving stock (Show)

$(deriveFromJSON defaultOptions ''UserDataUpload)
