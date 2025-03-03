module Slacklinker.Linear.WebhookSpec where

import Data.Aeson (eitherDecode)
import JSONGolden (readGoldenJSONFile)
import Slacklinker.Extract.Types (ExtractableMessage (..))
import Slacklinker.Linear.Webhook
import TestImport
import Web.Slack.Experimental.Events.Types (Event (..), EventCallback (..), SlackWebhookEvent (..))

spec :: Spec
spec = describe "Linear webhooks" do
  it "ignores linear bot messages" do
    ev <- either error identity . eitherDecode @SlackWebhookEvent <$> readGoldenJSONFile @SlackWebhookEvent "linear"
    case ev of
      (EventEventCallback (EventCallback {event = EventMessage msg})) ->
        shouldIgnore (extractData msg) `shouldBe` True
      _ -> error "wrong event type"
