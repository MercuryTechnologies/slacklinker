module Slacklinker.Handler.TestData where

import Web.Slack.Conversation (ConversationId (..))
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Types (TeamId (..), UserId (..))
import Slacklinker.Import
import Data.StringVariants (unsafeMkNonEmptyText)
import Slacklinker.Handler.TestUtils

ts1 :: Text
ts1 = "1663971111.111111"

ts2 :: Text
ts2 = "1663972222.222222"

-- real production event logged in test workspace
forwardedMessageEvent :: MessageEvent
forwardedMessageEvent =
  MessageEvent
    { blocks = Nothing,
      channel = ConversationId {unConversationId = "C07KHDGQ7K3"},
      text = "",
      channelType = Channel,
      files = Nothing,
      user = UserId {unUserId = "U043H11ES4V"},
      ts = "1725646557.141529",
      threadTs = Nothing,
      appId = Nothing,
      botId = Nothing,
      attachments =
        Just
          [ MessageAttachment
              { fallback = Just "[September 6th, 2024 11:15 AM] lev: this message was forwarded",
                color = Just "D0D0D0",
                pretext = Nothing,
                authorName = Just "lev",
                authorLink = Just "https://myworkspace.slack.com/team/U043H11ES4V",
                authorIcon = Just "https://avatars.slack-edge.com/2023-07-22/5607926127815_66b9d49327ec16a887e5_48.png",
                title = Nothing,
                titleLink = Nothing,
                text = Just "this message was forwarded",
                fields = Nothing,
                imageUrl = Nothing,
                thumbUrl = Nothing,
                footer = Just "Slack Conversation",
                footerIcon = Nothing,
                ts = Just "1725646540.483529",
                isMsgUnfurl = Just True,
                messageBlocks =
                  Just
                    [ AttachmentMessageBlock
                        { team = TeamId {unTeamId = "T0123"},
                          channel = ConversationId {unConversationId = "C043YJGBY49"},
                          ts = "1725646540.483529",
                          message =
                            AttachmentMessageBlockMessage
                              { blocks =
                                  [ SlackBlockRichText (RichText
                                      { blockId = Just (unsafeMkNonEmptyText "/dNUU"),
                                        elements = [RichTextSectionItemRichText [RichItemText "this message was forwarded" (RichStyle {rsBold = False, rsItalic = False})]]
                                      })
                                  ]
                              }
                        }
                    ],
                authorId = Just (UserId {unUserId = "U043H11ES4V"}),
                channelId = Just (ConversationId {unConversationId = "C043YJGBY49"}),
                channelTeam = Just (TeamId {unTeamId = "T0123"}),
                isAppUnfurl = Nothing,
                appUnfurlUrl = Nothing,
                fromUrl = Just "https://myworkspace.slack.com/archives/C043YJGBY49/p1725646540483529"
              }
          ]
    }

-- synthetic event, modified from above, with an attachment that has a URL, but wasnt a forwarded message (no fromurl)
attachedUrlEvent :: MessageEvent
attachedUrlEvent =
  MessageEvent
    { blocks = Nothing,
      channel = ConversationId {unConversationId = "C07KHDGQ7K3"},
      text = "",
      channelType = Channel,
      files = Nothing,
      user = UserId {unUserId = "U043H11ES4V"},
      ts = "1725646557.141529",
      threadTs = Nothing,
      appId = Nothing,
      botId = Nothing,
      attachments =
        Just
          [ MessageAttachment
              { fallback = Just "[September 6th, 2024 11:15 AM] lev: this message was forwarded",
                color = Just "D0D0D0",
                pretext = Nothing,
                authorName = Just "lev",
                authorLink = Just "https://myworkspace.slack.com/team/U043H11ES4V",
                authorIcon = Just "https://avatars.slack-edge.com/2023-07-22/5607926127815_66b9d49327ec16a887e5_48.png",
                title = Nothing,
                titleLink = Nothing,
                text = Just "this message was forwarded",
                fields = Nothing,
                imageUrl = Nothing,
                thumbUrl = Nothing,
                footer = Just "Slack Conversation",
                footerIcon = Nothing,
                ts = Just "1725646540.483529",
                isMsgUnfurl = Just True,
                messageBlocks =
                  Just
                    [ AttachmentMessageBlock
                        { team = TeamId {unTeamId = "T0123"},
                          channel = ConversationId {unConversationId = "C043YJGBY49"},
                          ts = "1725646540.483529",
                          message =
                            AttachmentMessageBlockMessage
                              { blocks =
                                  [ SlackBlockRichText (urlRichText "https://myworkspace.slack.com/archives/C043YJGBY49/p1725646540483529")
                                  ]
                              }
                        }
                    ],
                authorId = Just (UserId {unUserId = "U043H11ES4V"}),
                channelId = Just (ConversationId {unConversationId = "C043YJGBY49"}),
                channelTeam = Just (TeamId {unTeamId = "T0123"}),
                isAppUnfurl = Nothing,
                appUnfurlUrl = Nothing,
                fromUrl = Nothing
              }
          ]
    }

