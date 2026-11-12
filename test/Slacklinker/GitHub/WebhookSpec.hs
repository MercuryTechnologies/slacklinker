module Slacklinker.GitHub.WebhookSpec where

import Data.Set qualified as Set
import Slacklinker.GitHub.Types (PrRef (..))
import Slacklinker.GitHub.Webhook (singleTrackableRef)
import TestImport

spec :: Spec
spec = describe "Choosing the PR to track" do
  let known = Set.fromList [("foo", "bar"), ("foo", "baz")]
      a = PrRef "foo" "bar" 1
      b = PrRef "foo" "baz" 2
      untracked = PrRef "other" "repo" 3
  it "tracks a single PR" $ singleTrackableRef known [a] `shouldBe` Just a
  it "ignores PRs from repos without webhooks" $ singleTrackableRef known [untracked, a] `shouldBe` Just a
  it "skips messages with several trackable PRs" $ singleTrackableRef known [a, b] `shouldBe` Nothing
  it "skips messages with none" $ singleTrackableRef known [untracked] `shouldBe` Nothing
