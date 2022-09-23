module Slacklinker.Slack.ConversationsJoinSpec (spec) where

import JSONGolden
import Slacklinker.Slack.ConversationsJoin
import TestImport

spec :: Spec
spec = describe "Conversation join method" do
  describe "Response FromJSON" do
    mapM_ (oneGoldenTest @ConversationsJoinResponse) ["test"]
