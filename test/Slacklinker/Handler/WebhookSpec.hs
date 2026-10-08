module Slacklinker.Handler.WebhookSpec (spec) where

import Control.Monad.Fail (fail)
import Data.Aeson (Value, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither)
import Database.Persist
import Slacklinker.App (HasApp, runAppM, runDB)
import Slacklinker.Handler.TestData
import Slacklinker.Handler.TestUtils
import Slacklinker.Handler.Webhook (handleMessage)
import Slacklinker.Models
import Slacklinker.SplitUrl (SlackUrlParts (..), splitSlackUrl)
import TestApp
import TestImport
import TestUtils (createWorkspace)
import Web.Slack.Experimental.Blocks
import Web.Slack.Experimental.Events.Types
import Web.Slack.Types

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

    forM_
      [ ("list", RichTextSectionItemList . pure . RichTextSection)
      , ("quote", RichTextSectionItemQuote)
      , ("preformatted block", RichTextSectionItemPreformatted)
      ]
      \(name, container) ->
        it ("can find a link in a rich-text " <> name) \app -> do
          runAppM app do
            (wsId, teamId) <- createWorkspace
            let (url, parts) = sampleUrl
                RichText {elements = [RichTextSectionItemRichText (RichTextSection items)]} = urlRichText url
                block = SlackBlockRichText $ RichText {blockId = Nothing, elements = [container items]}
                msg = messageEventWithBlocks ts1 [block]
            handleMessage msg teamId

            Just (Entity rtId _) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs
            links <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
            liftIO $ map ((.messageTs) . entityVal) links `shouldBe` [msg.ts]

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
            AttachmentMessageBlock {message = AttachmentMessageBlockMessage {blocks = [SlackBlockRichText rt]}} = attachmentMessageBlock
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
          runDB
            $ getBy
            $ UniqueRepliedThread
              wsId
              childUrlParts.channelId
              (fromJust childUrlParts.threadTs)

        allThreads <- runDB $ selectList @RepliedThread [] []
        liftIO $ length allThreads `shouldBe` 1
        pure ()

  withApp $ describe "Rich message mentions" do
    forM_ ["rich_text_section", "rich_text_list", "rich_text_quote", "rich_text_preformatted"] \container ->
      forM_ [False, True] \inAttachment ->
        it ("backlinks a URL-less mention in " <> unpack container <> if inAttachment then " inside an attachment" else "") \app -> do
          let (_, parts) = sampleUrl
              block = richTextBlock container [mention parts Nothing]
          msg <-
            if inAttachment
              then decodeMessage [] [attachmentWithBlock block]
              else decodeMessage [block] []
          runAppM app do
            (wsId, teamId) <- createWorkspace
            handleMessage msg teamId
            Just (Entity rtId _) <- runDB $ getBy $ UniqueRepliedThread wsId parts.channelId parts.messageTs
            [Entity _ linkedMessage] <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
            Just (Entity channelId _) <- runDB $ getBy $ UniqueJoinedChannel wsId msg.channel
            liftIO do
              linkedMessage.joinedChannelId `shouldBe` channelId
              linkedMessage.messageTs `shouldBe` msg.ts
              linkedMessage.threadTs `shouldBe` Nothing
              linkedMessage.sent `shouldBe` False

    forM_ [False, True] \inAttachment ->
      it ("keeps mention thread metadata when its URL omits it" <> if inAttachment then " in an attachment" else "") \app -> do
        let (_, parts) = sampleUrlToChild
            urlWithoutThread = "https://jadeapptesting.slack.com/archives/C045V0VJT16/p1668735634647249"
            block = richTextBlock "rich_text_section" [mention parts (Just urlWithoutThread)]
        msg <-
          if inAttachment
            then decodeMessage [] [attachmentWithBlock block]
            else decodeMessage [block] []
        runAppM app do
          (wsId, teamId) <- createWorkspace
          handleMessage msg teamId
          threads <- runDB $ selectList [RepliedThreadWorkspaceId ==. wsId] []
          liftIO $ map ((.threadTs) . entityVal) threads `shouldBe` [fromJust parts.threadTs]

    it "records one backlink when links and mentions target the same thread" \app -> do
      let (parentUrl, parentParts) = sampleUrl
          (childUrl, childParts) = sampleUrlToChild
          linkItem url = Aeson.object ["type" .= ("link" :: Text), "url" .= url]
          block = richTextBlock "rich_text_section" [mention childParts Nothing, linkItem childUrl, linkItem parentUrl]
      msg <- decodeMessage [block] [attachmentWithBlock $ richTextBlock "rich_text_quote" [mention childParts (Just childUrl)]]
      runAppM app do
        (wsId, teamId) <- createWorkspace
        handleMessage msg teamId
        [Entity rtId thread] <- runDB $ selectList [RepliedThreadWorkspaceId ==. wsId] []
        links <- runDB $ selectList [LinkedMessageRepliedThreadId ==. rtId] []
        liftIO do
          thread.conversationId `shouldBe` parentParts.channelId
          thread.threadTs `shouldBe` parentParts.messageTs
          map ((.messageTs) . entityVal) links `shouldBe` [msg.ts]

    let (_, parentParts) = sampleUrl
        (_, childParts) = sampleUrlToChild
    forM_
      [ ("parent", parentParts, childParts.messageTs, Just parentParts.messageTs)
      , ("child", childParts, parentParts.messageTs, Nothing)
      , ("sibling", childParts, ts1, Just parentParts.messageTs)
      , ("message itself", parentParts, parentParts.messageTs, Nothing)
      ]
      \(name, destination, sourceTs, sourceThreadTs) ->
        it ("ignores a mention of a " <> name <> " in the same thread") \app -> do
          MessageEvent {..} <- decodeMessage [richTextBlock "rich_text_section" [mention destination Nothing]] []
          let msg = MessageEvent {channel = destination.channelId, ts = sourceTs, threadTs = sourceThreadTs, ..}
          runAppM app do
            (wsId, teamId) <- createWorkspace
            handleMessage msg teamId
            threads <- runDB $ selectList [RepliedThreadWorkspaceId ==. wsId] []
            liftIO $ length threads `shouldBe` 0

-- Decode the wire representation so tests cover both the dependency's parser
-- and extraction, including the separate attachment decoding path.
decodeMessage :: [Value] -> [Value] -> IO MessageEvent
decodeMessage blocks attachments =
  either fail pure
    $ parseEither Aeson.parseJSON
    $ Aeson.object
      [ "channel" .= ("C043YJGBY49" :: Text)
      , "channel_type" .= ("channel" :: Text)
      , "user" .= ("U043H11ES4V" :: Text)
      , "ts" .= ts1
      , "text" .= ("a message reference" :: Text)
      , "blocks" .= blocks
      , "attachments" .= attachments
      ]

mention :: SlackUrlParts -> Maybe Text -> Value
mention parts url =
  Aeson.object
    $ [ "type" .= ("message_mention" :: Text)
      , "channel_id" .= parts.channelId
      , "message_ts" .= parts.messageTs
      ]
    <> maybe [] (\ts -> ["thread_ts" .= ts]) parts.threadTs
    <> maybe [] (\u -> ["url" .= u]) url

richTextBlock :: Text -> [Value] -> Value
richTextBlock container items =
  Aeson.object
    [ "type" .= ("rich_text" :: Text)
    , "elements"
        .= [ Aeson.object
               [ "type" .= container
               , "elements"
                   .= if container == "rich_text_list"
                     then [Aeson.object ["type" .= ("rich_text_section" :: Text), "elements" .= items]]
                     else items
               ]
           ]
    ]

attachmentWithBlock :: Value -> Value
attachmentWithBlock block =
  Aeson.object
    [ "message_blocks"
        .= [ Aeson.object
               [ "team" .= ("T0123" :: Text)
               , "channel" .= ("C043YJGBY49" :: Text)
               , "ts" .= ts2
               , "message" .= Aeson.object ["blocks" .= [block]]
               ]
           ]
    ]
