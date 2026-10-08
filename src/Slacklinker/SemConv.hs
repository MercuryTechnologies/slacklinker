{- | Slacklinker-specific OpenTelemetry attribute names.

Keep names and their meanings here so handlers share the same conventions.
Prefer upstream semantic conventions when they cover a concept; this module
defines the application's custom attributes. Existing exported names are
preserved, including the different schemas for decoded and unknown events.

Import this module qualified as @SemConv@.
-}
module Slacklinker.SemConv where

import Slacklinker.Prelude (Text)

-- | Slack workspace ID associated with the operation.
slackTeamId :: Text
slackTeamId = "slack.team.id"

-- | Haskell constructor of the verified outer webhook envelope.
slackWebhookConstructor :: Text
slackWebhookConstructor = "slack.webhook.constructor"

-- | Haskell constructor of the decoded callback event.
slackEventConstructor :: Text
slackEventConstructor = "slack.event.constructor"

-- | Slack event type, such as message or channel_created.
slackEventType :: Text
slackEventType = "slack.event.type"

-- | Slack message subtype, when present.
slackEventSubtype :: Text
slackEventSubtype = "slack.event.subtype"

-- | Conversation ID on a decoded message or conversation event.
slackConversationId :: Text
slackConversationId = "slack.conversation.id"

-- | App ID associated with the event.
slackEventAppId :: Text
slackEventAppId = "slack.event.appId"

-- | Bot ID associated with the event.
slackEventBotId :: Text
slackEventBotId = "slack.event.botId"

-- | Slack timestamp identifying the deleted message.
slackEventDeletedTs :: Text
slackEventDeletedTs = "slack.event.deletedTs"

-- | Channel ID on a channel lifecycle event.
slackChannelId :: Text
slackChannelId = "slack.channel.id"

-- | Channel name on a channel creation event.
slackChannelName :: Text
slackChannelName = "slack.channel.name"

-- | Context workspace ID on a channel creation event.
slackChannelTeamId :: Text
slackChannelTeamId = "slack.channel.team.id"

-- | User ID of the actor on a channel-left event.
slackChannelActorId :: Text
slackChannelActorId = "slack.channel.actor.id"

-- | User ID on a decoded app-home-opened event.
slackEventUserId :: Text
slackEventUserId = "slack.event.user.id"

-- | Whether the event contains a nonempty attachment list.
slackEventHasAttachments :: Text
slackEventHasAttachments = "slack.event.hasAttachments"

-- | Number of attachments on the event.
slackEventNumAttachments :: Text
slackEventNumAttachments = "slack.event.numAttachments"

-- | Whether any decoded attachment is marked as a message unfurl.
slackEventAttachmentsHasMsgUnfurl :: Text
slackEventAttachmentsHasMsgUnfurl = "slack.event.attachments.hasMsgUnfurl"

-- | Whether any attachment lacks message blocks, including undecodable attachments.
slackEventAttachmentsHasNoMessageBlocks :: Text
slackEventAttachmentsHasNoMessageBlocks = "slack.event.attachments.hasNoMessageBlocks"

-- | Whether any attachment could not be decoded.
slackEventAttachmentsHasUndecodable :: Text
slackEventAttachmentsHasUndecodable = "slack.event.attachments.hasUndecodable"

-- | Raw user ID on an unknown event; preserves the existing unknown-event schema.
slackUnknownEventUserId :: Text
slackUnknownEventUserId = "slack.event.userId"

-- | Raw channel ID on an unknown event.
slackUnknownEventChannelId :: Text
slackUnknownEventChannelId = "slack.event.channelId"

-- | Raw Slack timestamp on an unknown event.
slackUnknownEventTs :: Text
slackUnknownEventTs = "slack.event.ts"

-- | Raw team field on an unknown event.
slackUnknownEventTeam :: Text
slackUnknownEventTeam = "slack.event.team"

-- | Sender operation's destination channel ID; preserves the existing sender schema.
slackChannel :: Text
slackChannel = "slack.channel"

-- | Parsed direct-message command, rendered using its Show instance.
slacklinkerCommand :: Text
slacklinkerCommand = "slacklinker.command"

-- | Whether the current Linear OAuth token needs refreshing.
linearOAuthRefreshing :: Text
linearOAuthRefreshing = "linear.oauth.refreshing"

-- | Current token expiry, rendered using its Show instance.
linearOAuthCurrentExpiresAt :: Text
linearOAuthCurrentExpiresAt = "linear.oauth.current_expires_at"

-- | Refreshed token expiry, rendered using its Show instance.
linearOAuthTokenExpiresAt :: Text
linearOAuthTokenExpiresAt = "linear.oauth.token.expires_at"

-- | Token type returned by Linear, or ? when absent.
linearOAuthTokenType :: Text
linearOAuthTokenType = "linear.oauth.token.token_type"

-- | OAuth error returned by Linear.
linearOAuthTokenError :: Text
linearOAuthTokenError = "linear.oauth.token.error"

-- | OAuth error description returned by Linear.
linearOAuthTokenErrorDescription :: Text
linearOAuthTokenErrorDescription = "linear.oauth.token.error.description"

-- | OAuth error URI returned by Linear.
linearOAuthTokenErrorUri :: Text
linearOAuthTokenErrorUri = "linear.oauth.token.error.uri"

-- | Verified webhook body that failed decoding; also the prefix for its flattened fields.
webhookPayload :: Text
webhookPayload = "payload"
