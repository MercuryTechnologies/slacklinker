module Slacklinker.Handler.Webhook.TypesSpec (spec) where

import JSONGolden
import Slacklinker.Handler.Webhook.Types (SlackWebhookEvent)
import TestImport

spec :: Spec
spec = describe "Types for Slack webhooks" do
  describe "SlackWebhookEvent" do
    describe "FromJSON" do
      mapM_
        (oneGoldenTest @SlackWebhookEvent)
        [ "messageExample"
        , "messageChange"
        , "link"
        , "botMessage"
        , "joinChannel"
        , "createChannel"
        , "messageIm"
        , "slackbotIm"
        , "channel_left"
        ]
