module Slacklinker.Sender.InternalSpec (spec) where

import Control.Monad.Fail (fail)
import Data.Aeson.KeyMap qualified as KeyMap
import Slacklinker.Sender.Internal
import TestImport
import Web.Slack.Common (ResponseSlackError (..), SlackClientError (..))
import Web.Slack.Types (ConversationId (..))

spec :: Spec
spec = describe "Slack request error context" $ do
  let channel = ConversationId "C0123456789"

  it "channel_not_found identifies the request when joining a channel" $ do
    message <- requestErrorMessage "channel_not_found" (JoinConversation channel)
    message `shouldEndWith` " while joining channel https://debug-workspace.slack.com/archives/C0123456789"

  it "channel_not_found identifies the request when posting a message" $ do
    message <- requestErrorMessage "channel_not_found" (PostMessage PostMessageContext {channel, threadTs = Nothing})
    message `shouldEndWith` " while posting message in channel https://debug-workspace.slack.com/archives/C0123456789"

  it "channel_not_found identifies the request when replying to a thread" $ do
    message <- requestErrorMessage "channel_not_found" (PostMessage PostMessageContext {channel, threadTs = Just "1730339894.000001"})
    message `shouldEndWith` " while replying to thread https://debug-workspace.slack.com/archives/C0123456789/p1730339894000001"

  it "channel_not_found identifies the request when updating a message" $ do
    message <- requestErrorMessage "channel_not_found" (UpdateMessage UpdateMessageContext {channel, messageTs = "1730339895.000002"})
    message `shouldEndWith` " while updating message https://debug-workspace.slack.com/archives/C0123456789/p1730339895000002"

  it "channel_not_found falls back to the channel URL when the message timestamp is invalid" $ do
    message <- requestErrorMessage "channel_not_found" (UpdateMessage UpdateMessageContext {channel, messageTs = "invalid-ts"})
    message `shouldEndWith` " while updating message https://debug-workspace.slack.com/archives/C0123456789"

  it "not_in_channel identifies the request when joining a channel" $ do
    message <- requestErrorMessage "not_in_channel" (JoinConversation channel)
    message `shouldEndWith` " while joining channel https://debug-workspace.slack.com/archives/C0123456789"

  it "not_in_channel identifies the request when posting a message" $ do
    message <- requestErrorMessage "not_in_channel" (PostMessage PostMessageContext {channel, threadTs = Nothing})
    message `shouldEndWith` " while posting message in channel https://debug-workspace.slack.com/archives/C0123456789"

  it "not_in_channel identifies the request when replying to a thread" $ do
    message <- requestErrorMessage "not_in_channel" (PostMessage PostMessageContext {channel, threadTs = Just "1730339894.000001"})
    message `shouldEndWith` " while replying to thread https://debug-workspace.slack.com/archives/C0123456789/p1730339894000001"

  it "not_in_channel identifies the request when updating a message" $ do
    message <- requestErrorMessage "not_in_channel" (UpdateMessage UpdateMessageContext {channel, messageTs = "1730339895.000002"})
    message `shouldEndWith` " while updating message https://debug-workspace.slack.com/archives/C0123456789/p1730339895000002"

  it "not_in_channel falls back to the channel URL when the message timestamp is invalid" $ do
    message <- requestErrorMessage "not_in_channel" (UpdateMessage UpdateMessageContext {channel, messageTs = "invalid-ts"})
    message `shouldEndWith` " while updating message https://debug-workspace.slack.com/archives/C0123456789"

  it "returns successful responses unchanged" $ do
    result <- withSlackRequestContext "debug-workspace" (JoinConversation channel) $ pure (Right ("joined" :: Text))
    result `shouldBe` "joined"

requestErrorMessage :: Text -> SlackRequestContext -> IO String
requestErrorMessage errorCode requestContext = do
  let original = SlackError $ ResponseSlackError errorCode (KeyMap.fromList [("detail", "retained")])
  result <- try $ withSlackRequestContext "debug-workspace" requestContext $ pure (Left original :: Either SlackClientError ())
  case result of
    Left (err :: SlackRequestError) -> do
      err.slackError `shouldBe` original
      pure $ displayException err
    Right () -> fail "Expected the Slack error to be thrown"
