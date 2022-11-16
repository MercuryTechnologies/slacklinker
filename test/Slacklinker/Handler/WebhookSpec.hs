module Slacklinker.Handler.WebhookSpec (spec) where

import Slacklinker.App (runAppM, runDB)
import Slacklinker.Handler.Webhook (handleMessage)
import TestApp
import TestImport
import TestUtils (createWorkspace)
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Types
import Slacklinker.SplitUrl (splitSlackUrl, SlackUrlParts(..))
import Slacklinker.Models
import Database.Persist

sampleUrl :: (Text, SlackUrlParts)
sampleUrl = (url, fromJust $ splitSlackUrl url)
  where
  url = "https://jadeapptesting.slack.com/archives/C043YJGBY49/p1663961604007869"

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

messageEventWithBlocks :: [SlackBlock] -> MessageEvent
messageEventWithBlocks blocks =
  MessageEvent
    { blocks
    , channel = ConversationId "C043YJGBY49"
    , text = "nobody looks at this"
    , channelType = Channel
    , user = UserId "U043H11ES4V"
    , ts = "1663978925.099999"
    , threadTs = Nothing
    , appId = Nothing
    , botId = Nothing
    }

spec :: Spec
spec = describe "Webhooks" do
  withApp $ describe "Should insert RepliedThread for a message" do
    it "simple link" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (url, parts) = sampleUrl
        let msg = messageEventWithBlocks [SlackBlockRichText . urlRichText $ url]
        handleMessage msg teamId

        -- FIXME: MonadFail instead of irrefutable pattern crimes
        ~(Just (Entity threadId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        ~[Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. threadId] []
        liftIO $ do
          theLink.conversationId `shouldBe` msg.channel
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False


        pure ()
      pure @IO ()
