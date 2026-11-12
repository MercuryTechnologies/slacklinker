module Slacklinker.GitHub.ExtractSpec where

import Slacklinker.GitHub.Extract (extractPrRefs)
import Slacklinker.GitHub.Types (PrRef (..), RepoName (..))
import TestImport

spec :: Spec
spec = describe "Extracting GitHub PR links" do
  let pr = PrRef (RepoName "foo" "bar.hs") 12
  it "finds a Slack-formatted link" do
    extractPrRefs "see <https://github.com/Foo/Bar.hs/pull/12|this>" `shouldBe` [pr]
  it "finds a bare link with a trailing path" do
    extractPrRefs "https://github.com/foo/bar.hs/pull/12/files#diff-abc" `shouldBe` [pr]
  it "matches any host case" do
    extractPrRefs "https://GitHub.com/foo/bar.hs/pull/12" `shouldBe` [pr]
  it "accepts www" do
    extractPrRefs "https://www.github.com/foo/bar.hs/pull/12" `shouldBe` [pr]
  it "dedupes and finds several" do
    extractPrRefs "https://github.com/foo/bar.hs/pull/12 https://github.com/foo/bar.hs/pull/12 https://github.com/a/b/pull/3"
      `shouldBe` [pr, PrRef (RepoName "a" "b") 3]
  it "ignores issues and other hosts" do
    extractPrRefs "https://github.com/foo/bar/issues/12 https://notgithub.com/foo/bar/pull/1" `shouldBe` []
