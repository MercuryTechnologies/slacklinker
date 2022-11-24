{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Slacklinker.Types where

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Database.Persist (PersistField)
import Database.Persist.Postgresql (PersistFieldSql)
import Network.HTTP.Media qualified as M
import Servant (Accept)
import Servant.API.ContentTypes (Accept (..), MimeUnrender (..))
import Slacklinker.Import
import Web.Slack.Experimental.RequestVerification

-- | JSON but it's not parsed
data JSONByteString

instance Accept JSONByteString where
  contentTypes _ =
    "application" M.// "json" M./: ("charset", "utf-8")
      NE.:| ["application" M.// "json"]

instance MimeUnrender JSONByteString ByteString where
  mimeUnrender _ = Right . toStrict

-- | Secret part of the app API credentials credentials for OAuth mode
newtype SlackClientSecret = SlackClientSecret Text
  deriving stock (Eq)
  deriving newtype (ToHttpApiData)

instance Show SlackClientSecret where
  show _ = "<SlackClientSecret>"

-- FIXME(jadel): the validation should be in here using the functions here but i dont
-- have time for that
instance FromJSON SlackSigningSecret where
  parseJSON = withText "SlackSigningSecret" (pure . SlackSigningSecret . encodeUtf8)

newtype SlackToken = SlackToken {unSlackToken :: Text}
  deriving stock (Eq)
  deriving newtype (FromJSON, PersistField, PersistFieldSql)

instance Show SlackToken where
  show _ = "<SlackToken>"

-- | Slack emoji name, without surrounding colons (e.g. @slightly_smiling_face@)
newtype Emoji = Emoji {unEmoji :: Text}
  deriving stock (Eq)
  deriving newtype (FromJSON, PersistField, PersistFieldSql)
