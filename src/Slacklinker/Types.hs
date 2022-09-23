{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Types where

import Data.Aeson
import Data.List.NonEmpty qualified as NE
import Database.Persist (PersistField)
import Database.Persist.Postgresql (PersistFieldSql)
import Network.HTTP.Media qualified as M
import Servant (Accept)
import Servant.API.ContentTypes (Accept (..), MimeUnrender (..))
import Slacklinker.Import

-- | JSON but it's not parsed
data JSONByteString

instance Accept JSONByteString where
  contentTypes _ =
    "application" M.// "json" M./: ("charset", "utf-8")
      NE.:| ["application" M.// "json"]

instance MimeUnrender JSONByteString ByteString where
  mimeUnrender _ = Right . toStrict


-- | Slack generated Signing Secret placed into configuration.
-- See https://api.slack.com/authentication/verifying-requests-from-slack#signing_secrets_admin_page
newtype SlackSigningSecret
  = SlackSigningSecret ByteString
  deriving stock (Eq)

instance Show SlackSigningSecret where
  show _ = "<SlackSigningSecret>"

newtype SlackClientSecret = SlackClientSecret Text
  deriving stock (Eq)
  deriving newtype (ToHttpApiData)

instance Show SlackClientSecret where
  show _ = "<SlackClientSecret>"

newtype SlackSignature = SlackSignature ByteString
  deriving newtype (Eq, Show)

newtype SlackRequestTimestamp = SlackRequestTimestamp ByteString
  deriving newtype (Eq, Show)

-- FIXME(jadel): the validation should be in here using the functions here but i dont
-- have time for that
instance FromHttpApiData SlackRequestTimestamp where
  parseQueryParam _ = error "SlackRequestTimestamp should not be in a query param"
  parseUrlPiece _ = error "SlackRequestTimestamp should not be in a url piece"
  parseHeader = Right . SlackRequestTimestamp

instance FromHttpApiData SlackSignature where
  parseQueryParam _ = error "SlackSignature should not be in a query param"
  parseUrlPiece _ = error "SlackSignature should not be in a url piece"
  parseHeader = Right . SlackSignature

instance FromJSON SlackSigningSecret where
  parseJSON = withText "SlackSigningSecret" (pure . SlackSigningSecret . encodeUtf8)

-- * Event types

data UrlVerificationPayload = UrlVerificationPayload
  { challenge :: Text
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''UrlVerificationPayload)

newtype EventId = EventId {unEventId :: Text}
  deriving newtype (FromJSON)
  deriving stock (Show)

newtype MessageId = MessageId {unMessageId :: Text}
  deriving newtype (FromJSON, PersistField, PersistFieldSql)
  deriving stock (Show, Eq)

data ChannelType = Channel | Group | Im
  deriving stock (Show, Eq)

-- wtf, I had to use deriveJSON, not deriveFromJSON to get the right decoding
-- here?
$(deriveJSON snakeCaseOptions ''ChannelType)

newtype SlackToken = SlackToken {unSlackToken :: Text}
  deriving stock (Eq)
  deriving newtype (FromJSON, PersistField, PersistFieldSql)

instance Show SlackToken where
  show _ = "<SlackToken>"
