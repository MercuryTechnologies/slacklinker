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
import Slacklinker.Handler.TestData
import Slacklinker.Handler.TestUtils

doLink :: (HasApp m, MonadUnliftIO m) => TeamId -> Text -> Text -> m MessageEvent
doLink teamId ts url = do
  let msg = messageEventWithBlocks ts [SlackBlockRichText . urlRichText $ url]
  handleMessage msg teamId
  pure msg

doBotLink :: (HasApp m, MonadUnliftIO m) => TeamId -> Text -> Text -> m BotMessageEvent
doBotLink teamId ts url = do
  let msg = botMessageEventWithBlocks ts [SlackBlockRichText . urlRichText $ url]
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
        (Just (Entity _ userData)) <- runDB . getBy $ UniqueKnownUser wsId msg.user
        liftIO $ userData.emoji `shouldBe` Nothing

  withApp $ describe "Should insert RepliedThread for a message" do
    it "can deal with a simple link" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (url, parts) = sampleUrl
        msg <- doLink teamId ts1 url

        Just (Entity rtId _thread) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    it "can deal with a bot link" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (url, parts) = sampleUrl
        msg <- doBotLink teamId ts1 url

        (Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    it "can deal with a forwarded url" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let msg = forwardedMessageEvent
            Just [MessageAttachment {decoded = Just DecodedMessageAttachment {fromUrl = Just url}}] = msg.attachments
            parts = fromJust $ splitSlackUrl url

        handleMessage msg teamId

        (Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    it "can deal with an attached url" \app -> do
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let msg = attachedUrlEvent
            Just [MessageAttachment {decoded = Just DecodedMessageAttachment {messageBlocks = Just [attachmentMessageBlock]}}] = msg.attachments
            AttachmentMessageBlock {message = AttachmentMessageBlockMessage { blocks = [SlackBlockRichText rt]}} = attachmentMessageBlock
            Just url = richTextToMaybeUrl rt
            parts = fromJust $ splitSlackUrl url

        handleMessage msg teamId

        (Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- This should name the message that triggered slacklinker
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False
    it "can find URLs in undecodable attachments" \app -> do
      -- in this case, slacklinker will run the url detection parser on the raw json
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let msg = messageWithUndecodableAttachment
            Just [MessageAttachment {decoded = decodedAttachments}] = msg.attachments
        handleMessage msg teamId
      
        let expectedChannelId = ConversationId "C07KTH1T4CQ"
            expectedMessageTs = "1730339894.629249"

        (Just (Entity rtId _thread)) <- runDB $ getBy $ UniqueRepliedThread wsId expectedChannelId expectedMessageTs

        [Entity _ theLink] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        (Just (Entity channelId _)) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel

        liftIO $ do
          -- we check that decoding failed (if this fails, the test has been setup incorrectly)
          isNothing decodedAttachments `shouldBe` True
          theLink.joinedChannelId `shouldBe` channelId
          theLink.messageTs `shouldBe` msg.ts
          theLink.threadTs `shouldBe` Nothing
          theLink.sent `shouldBe` False

    it "will not link a message within the same thread" \app -> do
      -- the setup should be
      -- lev: heres a parent message 
      --  |-> (in thread) lev: i am linking to https://myworkspace.slack.com/archives/C045V0VJT16/p1725559477299859
      --
      -- where the parent message url is: https://myworkspace.slack.com/archives/C045V0VJT16/p1725559477299859
      -- and the thread message url is https://myworkspace.slack.com/archives/C045V0VJT16/p1725559485267619?thread_ts=1725559477.299859
      --
      -- importantly, parent.message_ts == child.thread_ts
      -- and parent.channel_id == child.channel_id
      runAppM app $ do
        (wsId, teamId) <- createWorkspace
        let (parentUrl, parentParts) = sampleUrl
            childMessage = messageEventWithBlocks ts1 [SlackBlockRichText . urlRichText $ parentUrl]
            childMessageWithThread = updateThreadTs childMessage (Just parentParts.messageTs)
            childMessageWithChannel = updateChannelId childMessageWithThread parentParts.channelId

        handleMessage childMessageWithChannel teamId

        -- We should not plan a reply to a thread that links to itself
        Nothing <- runDB $ getBy $ UniqueRepliedThread wsId parentParts.channelId parentParts.messageTs
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

        (Just _) <-
          runDB $
            getBy $
              UniqueRepliedThread
                wsId
                childUrlParts.channelId
                (fromJust childUrlParts.threadTs)

        allThreads <- runDB $ selectList @RepliedThread [] []
        liftIO $ length allThreads `shouldBe` 1
        pure ()
