module Slacklinker.Handler.TestData where

import Web.Slack.Conversation (ConversationId (..))
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Types (TeamId (..), UserId (..))
import Slacklinker.Import
import Data.StringVariants (unsafeMkNonEmptyText)
import Slacklinker.Handler.TestUtils
import Data.Aeson (Value(Object))
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.Vector qualified as V

emptyJsonObject :: Value
emptyJsonObject = Object KM.empty

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
              { decoded = Just $ DecodedMessageAttachment { 
                  fallback = Just "[September 6th, 2024 11:15 AM] lev: this message was forwarded",
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
                },
                raw = emptyJsonObject
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
              { decoded = Just $ DecodedMessageAttachment {
                  fallback = Just "[September 6th, 2024 11:15 AM] lev: this message was forwarded",
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
                },
                raw = emptyJsonObject
              }
          ]
    }

-- A PR was opened and the author linked a slack message in the PR description.
-- When this is posted to slack, the slack message link can be backlinked.
-- Note that we were actually able to decode parts of the message here, but
-- we use a general parser to find the slack message link from the raw aeson Value.
messageWithUndecodableAttachment :: MessageEvent
messageWithUndecodableAttachment =
              MessageEvent
                { blocks = Nothing
                , channel = ConversationId
                    { unConversationId = "C07LRFB3C8M" }
                , text = ""
                , channelType = Channel
                , files = Nothing
                , user = UserId
                    { unUserId = "U07M725KAD7" }
                , ts = "1730339959.644979"
                , threadTs = Nothing
                , appId = Just "A01BP7R4KNY"
                , botId = Just "B07LDR01Z63"
                , attachments = Just
                    [ MessageAttachment
                        { decoded = Nothing
                        , raw = A.Object
                            ( KM.fromList
                                [
                                    ( "actions"
                                    , A.Array $ V.fromList [ A.Object
                                            ( KM.fromList
                                                [
                                                    ( "id"
                                                    , A.String "1"
                                                    )
                                                ,
                                                    ( "name"
                                                    , A.String "comment"
                                                    )
                                                ,
                                                    ( "style"
                                                    , A.String ""
                                                    )
                                                ,
                                                    ( "text"
                                                    , A.String "Comment"
                                                    )
                                                ,
                                                    ( "type"
                                                    , A.String "button"
                                                    )
                                                ,
                                                    ( "value"
                                                    , A.String "{&amp;amp;quot;selectedOrg&amp;amp;quot;:&amp;amp;quot;myorg&amp;amp;quot;,&amp;amp;quot;selectedOrgId&amp;amp;quot;:140364112,&amp;amp;quot;selectedRepo&amp;amp;quot;:&amp;amp;quot;myrepo&amp;amp;quot;,&amp;amp;quot;selectedRepoId&amp;amp;quot;:669978765,&amp;amp;quot;number&amp;amp;quot;:8,&amp;amp;quot;htmlUrl&amp;amp;quot;:&amp;amp;quot;https://github.com/myorg/myrepo/pull/8&amp;amp;quot;,&amp;amp;quot;title&amp;amp;quot;:&amp;amp;quot;https://jadeapptesting.slack.com/archives/C07KTH1T4CQ/p1730339894629249&amp;amp;quot;}"
                                                    )
                                                ]
                                            )
                                        ]
                                    )
                                ,
                                    ( "callback_id"
                                    , A.String "pr-opened-interaction"
                                    )
                                ,
                                    ( "color"
                                    , A.String "36a64f"
                                    )
                                ,
                                    ( "fallback"
                                    , A.String "[myorg/myrepo] Pull request opened by ldub"
                                    )
                                ,
                                    ( "footer"
                                    , A.String "<https://github.com/myorg/myrepo|myorg/myrepo>"
                                    )
                                ,
                                    ( "footer_icon"
                                    , A.String "https://slack.github.com/static/img/favicon-neutral.png"
                                    )
                                ,
                                    ( "id"
                                    , A.Number 1.0
                                    )
                                ,
                                    ( "mrkdwn_in"
                                    , A.Array $ V.fromList [ A.String "text" ]
                                    )
                                ,
                                    ( "pretext"
                                    , A.String "Pull request opened by <https://github.com/ldub|ldub>"
                                    )
                                ,
                                    ( "text"
                                    , A.String "this is a fake pr with a link\n<https://jadeapptesting.slack.com/archives/C07KTH1T4CQ/p1730339894629249|https://jadeapptesting.slack.com/archives/C07KTH1T4CQ/p1730339894629249>"
                                    )
                                ,
                                    ( "title"
                                    , A.String "<https://github.com/myorg/myrepo/pull/8|#8 https://jadeapptesting.slack.com/archives/C07KTH1T4CQ/p1730339894629249>"
                                    )
                                ,
                                    ( "ts"
                                    , A.Number 1.730339957e9
                                    )
                                ]
                            )
                        }
                    ]
                }