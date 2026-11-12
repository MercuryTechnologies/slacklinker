module Slacklinker.GitHub.WebhookSpec where

import Data.Set qualified as Set
import Slacklinker.GitHub.Types (PrRef (..), RepoName (..))
import Slacklinker.GitHub.Webhook (singleTrackableRef)
import TestImport

spec :: Spec
spec = describe "Choosing the PR to track" do
  let known = Set.fromList [RepoName "foo" "bar", RepoName "foo" "baz"]
      a = PrRef (RepoName "foo" "bar") 1
      b = PrRef (RepoName "foo" "baz") 2
      untracked = PrRef (RepoName "other" "repo") 3
  it "tracks a single PR" $ singleTrackableRef known [a] `shouldBe` Just a
  it "ignores PRs from repos without webhooks" $ singleTrackableRef known [untracked, a] `shouldBe` Just a
  it "skips messages with several trackable PRs" $ singleTrackableRef known [a, b] `shouldBe` Nothing
  it "skips messages with none" $ singleTrackableRef known [untracked] `shouldBe` Nothing
