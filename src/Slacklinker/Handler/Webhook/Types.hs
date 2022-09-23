{-# LANGUAGE TemplateHaskell #-}

-- | == Design of Slacklinker webhooks
--
--   Slacklinker uses the [Slack Events API](https://api.slack.com/events) to
--   get real time notifications of messages.
--
--   Webhooks need to have their signature verified, which is done on the
--   endpoint handler. After this, we know that the messages were sent by Slack
--   and can be trusted.
--
--   These come in as a big sum type, depending on which subscriptions are in
--   effect. This code is probably going to be upstreamed to slack-web.
module Slacklinker.Handler.Webhook.Types where

import Data.Aeson
import Data.Time.Clock.System (SystemTime)
import Slacklinker.Import
import Slacklinker.Types (ChannelType, EventId, UrlVerificationPayload)
import Web.Slack.Experimental.Blocks (SlackBlock)
import Web.Slack.Types (ConversationId, TeamId, UserId)

-- <https://api.slack.com/events/message>
data MessageEvent = MessageEvent
  { blocks :: [SlackBlock]
  , channel :: ConversationId
  , text :: Text
  , channelType :: ChannelType
  , -- FIXME(jadel): clientMsgId??
    user :: UserId
  , ts :: Text
  , threadTs :: Maybe Text
  -- ^ Present if the message is in a thread
  , appId :: Maybe Text
  -- ^ Present if it's sent by an app
  --
  --   For mysterious reasons, this is NOT
  --   <https://api.slack.com/events/message/bot_message>, this is a regular
  --   message but with bot metadata. I Think it's because there *is* an
  --   associated user.
  --
  --   See @botMessage.json@ golden parser test.
  , botId :: Maybe Text
  -- ^ Present if it's sent by a bot user
  }
  deriving stock (Show)

-- <https://api.slack.com/events/message/message_changed>
--
-- FIXME(jadel): implement. This is mega jacked! in the normal message event
-- the channel is called "channel" but here it is called "channelId" and also
-- has a "channel_name" and "channel_team". what the hell
data MessageUpdateEvent = MessageUpdateEvent
  { message :: MessageEvent
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''MessageEvent)
$(deriveFromJSON snakeCaseOptions ''MessageUpdateEvent)

data CreatedChannel = CreatedChannel
  { id :: ConversationId
  , isChannel :: Bool
  , name :: Text
  , nameNormalized :: Text
  , creator :: UserId
  , created :: SystemTime
  , isShared :: Bool
  , isOrgShared :: Bool
  , -- what is this?
    contextTeamId :: TeamId
  }
  deriving stock (Show)

data ChannelCreatedEvent = ChannelCreatedEvent
  { channel :: CreatedChannel
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''CreatedChannel)
$(deriveFromJSON snakeCaseOptions ''ChannelCreatedEvent)

data ChannelLeftEvent = ChannelLeftEvent
  { actorId :: UserId
  , channel :: ConversationId
  , eventTs :: Text
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''ChannelLeftEvent)

data Event
  = EventMessage MessageEvent
  | EventMessageChanged
  | -- | Weird message of subtype channel_join. Undocumented??
    EventChannelJoinMessage
  | EventChannelCreated ChannelCreatedEvent
  | EventChannelLeft ChannelLeftEvent
  | EventUnknown Value
  deriving stock (Show)

instance FromJSON Event where
  parseJSON = withObject "MessageEvent" \obj -> do
    tag :: Text <- obj .: "type"
    subtype :: Maybe Text <- obj .:? "subtype"
    case (tag, subtype) of
      ("message", Nothing) -> EventMessage <$> parseJSON @MessageEvent (Object obj)
      ("message", Just "message_changed") -> pure EventMessageChanged
      ("message", Just "channel_join") -> pure EventChannelJoinMessage
      ("channel_created", Nothing) -> EventChannelCreated <$> parseJSON (Object obj)
      ("channel_left", Nothing) -> EventChannelLeft <$> parseJSON (Object obj)
      _ -> pure $ EventUnknown (Object obj)

data EventCallback = EventCallback
  { eventId :: EventId
  , teamId :: TeamId
  , eventTime :: SystemTime
  , event :: Event
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''EventCallback)

data SlackWebhookEvent
  = EventUrlVerification UrlVerificationPayload
  | EventEventCallback EventCallback
  | EventUnknownWebhook Value
  deriving stock (Show)

instance FromJSON SlackWebhookEvent where
  parseJSON = withObject "SlackWebhookEvent" \obj -> do
    tag :: Text <- obj .: "type"
    case tag of
      "url_verification" -> EventUrlVerification <$> parseJSON (Object obj)
      "event_callback" -> EventEventCallback <$> parseJSON (Object obj)
      _ -> pure $ EventUnknownWebhook (Object obj)

-- * Event responses

-- FIXME(jadel): could this be a typeclass in the future?

data UrlVerificationResponse = UrlVerificationResponse
  { challenge :: Text
  }
  deriving stock (Show)

$(deriveToJSON defaultOptions ''UrlVerificationResponse)
