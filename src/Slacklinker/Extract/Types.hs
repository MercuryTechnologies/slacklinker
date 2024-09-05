module Slacklinker.Extract.Types where

import Slacklinker.Import
import Web.Slack.Conversation (ConversationId (..))
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Files.Types (FileObject)
import Web.Slack.Types qualified as Slack (UserId (..))

data ExtractedMessageData = ExtractedMessageData
  { channelType :: ChannelType
  , channel :: ConversationId
  , user :: Slack.UserId
  , ts :: Text
  , threadTs :: Maybe Text
  , text :: Text
  , blocks :: Maybe [SlackBlock]
  , files :: Maybe [FileObject]
  }
  deriving stock (Show)

class ExtractableMessage m where
    extractData :: m -> ExtractedMessageData

instance ExtractableMessage MessageEvent where
    extractData MessageEvent {..} = ExtractedMessageData {..}

instance ExtractableMessage BotMessageEvent where
    extractData BotMessageEvent {..} = 
        ExtractedMessageData
          { -- a bit of a hack, but user ids and bot ids don't overlap
            user = Slack.UserId { unUserId = botId }
          , ..
          }
