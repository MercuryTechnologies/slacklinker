module Slacklinker.Linear.WebhookSpec where

import Data.Aeson (eitherDecode)
import JSONGolden (readGoldenJSONFile)
import Slacklinker.Extract.Types (ExtractableMessage (..), ExtractedMessageData (..))
import Slacklinker.Linear.Webhook
import TestImport
import Web.Slack.Experimental.Events.Types (Event (..), EventCallback (..), SlackWebhookEvent (..))

goldenMessage :: Text -> IO SlackWebhookEvent
goldenMessage name = either error identity . eitherDecode @SlackWebhookEvent <$> readGoldenJSONFile @SlackWebhookEvent name

spec :: Spec
spec = describe "Linear webhooks" do
  it "ignores linear bot messages" do
    event <- goldenMessage "linear"
    case event of
      (EventEventCallback (EventCallback {event = EventMessage msg})) ->
        shouldIgnore (extractData msg) `shouldBe` True
      _ -> error "wrong event type"
  it "ignores linear ticket creation" do
    event <- goldenMessage "linear-new-ticket"
    case event of
      (EventEventCallback (EventCallback {event = EventBotMessage msg})) ->
        shouldIgnore (extractData msg) `shouldBe` True
      _ -> error "wrong event type"
  it "preserves display-name precedence over the Linear app ID" do
    event <- goldenMessage "linear"
    case event of
      EventEventCallback EventCallback {event = EventMessage msg} -> do
        let message = (extractData msg) {botDisplayName = Just "Other bot", appId = Just "AEMQ3Q4F4"}
        shouldIgnore message `shouldBe` False
      _ -> error "wrong event type"
