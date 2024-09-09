module Slacklinker.Handler.TestUtils where

import Web.Slack.Conversation (ConversationId (..))
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Types (UserId (..))
import Slacklinker.Import
import TestImport
import Slacklinker.SplitUrl (SlackUrlParts (..), splitSlackUrl)

sampleUrl :: (Text, SlackUrlParts)
sampleUrl = (url, fromJust $ splitSlackUrl url)
  where
    url = "https://jadeapptesting.slack.com/archives/C045V0VJT16/p1665014817153719"

sampleUrlToChild :: (Text, SlackUrlParts)
sampleUrlToChild = (url, fromJust $ splitSlackUrl url)
  where
    url = "https://jadeapptesting.slack.com/archives/C045V0VJT16/p1668735634647249?thread_ts=1665014817.153719&cid=C045V0VJT16"

urlRichText :: Text -> RichText
urlRichText url =
  RichText
    { blockId = Nothing
    , elements =
        [ RichTextSectionItemRichText
            [ RichItemLink
                ( RichLinkAttrs
                    { style = RichStyle {rsBold = False, rsItalic = False}
                    , url
                    , text = Nothing
                    }
                )
            ]
        ]
    }

richTextToMaybeUrl :: RichText -> Maybe Text
richTextToMaybeUrl rt = listToMaybe $ concatMap fromRichSectionItem rt.elements
  where
    fromRichSectionItem (RichTextSectionItemRichText items) = concatMap fromRichItem items
    fromRichSectionItem _ = []

    fromRichItem (RichItemLink attrs) = [url attrs]
    fromRichItem _ = []

messageEventWithBlocks :: Text -> [SlackBlock] -> MessageEvent
messageEventWithBlocks ts blocks =
  MessageEvent
    { blocks = Just blocks
    , channel = ConversationId "C043YJGBY49"
    , text = "nobody looks at this"
    , channelType = Channel
    , user = UserId "U043H11ES4V"
    , ts
    , files = Nothing
    , threadTs = Nothing
    , appId = Nothing
    , botId = Nothing
    , attachments = Nothing
    }

botMessageEventWithBlocks :: Text -> [SlackBlock] -> BotMessageEvent
botMessageEventWithBlocks ts blocks =
  BotMessageEvent
    { blocks = Just blocks
    , channel = ConversationId "C043YJGBY49"
    , text = "nobody looks at this"
    , channelType = Channel
    , ts
    , files = Nothing
    , threadTs = Nothing
    , appId = Just "XYZ123"
    , botId = "123XYZ"
    , attachments = Nothing
    }

-- XXX: lol, DuplicateRecordFields makes update syntax not work if two fields
-- of the same name are in scope
updateThreadTs :: MessageEvent -> Maybe Text -> MessageEvent
updateThreadTs MessageEvent {..} newThreadTs = MessageEvent {threadTs = newThreadTs, ..}

updateChannelId :: MessageEvent -> ConversationId -> MessageEvent
updateChannelId MessageEvent {..} newChannelId = MessageEvent {channel = newChannelId, ..}