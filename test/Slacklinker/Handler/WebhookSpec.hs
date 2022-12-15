module Slacklinker.Handler.WebhookSpec (spec) where

import Database.Persist
import Slacklinker.App (HasApp, runAppM, runDB)
import Slacklinker.Handler.Webhook (handleMessage)
import Slacklinker.Models
import Slacklinker.SplitUrl (SlackUrlParts (..), splitSlackUrl)
import TestApp
import TestImport
import TestUtils (createWorkspace)
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Types

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

ts1 :: Text
ts1 = "1663971111.111111"

ts2 :: Text
ts2 = "1663972222.222222"

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
    }

-- XXX: lol, DuplicateRecordFields makes update syntax not work if two fields
-- of the same name are in scope
updateThreadTs :: MessageEvent -> Maybe Text -> MessageEvent
updateThreadTs MessageEvent {..} newThreadTs = MessageEvent {threadTs = newThreadTs, ..}

doLink :: (HasApp m, MonadIO m) => TeamId -> Text -> Text -> m MessageEvent
doLink teamId ts url = do
  let msg = messageEventWithBlocks ts [SlackBlockRichText . urlRichText $ url]
  handleMessage msg teamId
  pure msg

spec :: Spec
spec = do
  withApp $ describe "User insertion" do
    it "inserts a user when a link is sent" \app -> do
      runAppM app do
        (wsId, teamId) <- createWorkspace
        let (url, _parts) = sampleUrl
        msg <- doLink teamId ts1 url
        ~(Just (Entity _ userData)) <- runDB . getBy $ UniqueKnownUser wsId msg.user
        liftIO $ userData.emoji `shouldBe` Nothing

  withApp $ describe "Should insert RepliedThread for a message" do
    it "can deal with a simple link" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (url, parts) = sampleUrl
        msg <- doLink teamId ts1 url

        -- FIXME: MonadFail instead of irrefutable pattern crimes
        ~(Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        ~[Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        ~(Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    it "will not link a message within the same thread" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (url, parts) = sampleUrl
        let msg =
              updateThreadTs
                (messageEventWithBlocks ts1 [SlackBlockRichText . urlRichText $ url])
                (Just parts.messageTs)

        handleMessage msg teamId

        -- We should not plan a reply to a thread that links to itself
        ~Nothing <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs
        pure ()

    it "will file a link to a message downthread as the same thread as linking the parent" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        -- Create a message linking the thread parent

        let (childUrl, childUrlParts) = sampleUrlToChild
        void $ doLink teamId ts1 childUrl

        let (parentUrl, parentUrlParts) = sampleUrl
        void $ doLink teamId ts2 parentUrl

        -- Verify the test data reproduces the expected condition
        liftIO $ childUrlParts.threadTs `shouldBe` Just parentUrlParts.messageTs

        ~(Just _) <-
          runDB $
            getBy $
              UniqueRepliedThread
                wsId
                childUrlParts.channelId
                (fromJust childUrlParts.threadTs)

        allThreads <- runDB $ selectList @RepliedThread [] []
        liftIO $ length allThreads `shouldBe` 1
        pure ()
