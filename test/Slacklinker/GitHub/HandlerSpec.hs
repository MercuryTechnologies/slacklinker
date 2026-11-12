module Slacklinker.GitHub.HandlerSpec where

import Data.Aeson (eitherDecode)
import Slacklinker.GitHub.Handler (GithubWebhookPayload (..), changesPrState, verifyGithubSignature)
import Slacklinker.GitHub.Types (GithubWebhookSecret (..))
import TestImport

spec :: Spec
spec = do
  describe "GitHub webhook signatures" do
    -- https://docs.github.com/en/webhooks/using-webhooks/validating-webhook-deliveries#testing-the-webhook-payload-validation
    let secret = GithubWebhookSecret "It's a Secret to Everybody"
        sig = "sha256=757107ea0eb2509fc211221cce984b8a37570b6d7586c22c46f4379c8b043e17"
    it "accepts GitHub's example" do
      verifyGithubSignature secret sig "Hello, World!" `shouldBe` True
    it "rejects a modified body" do
      verifyGithubSignature secret sig "Hello, World?" `shouldBe` False
    it "rejects a malformed header" do
      verifyGithubSignature secret "sha1=abc" "Hello, World!" `shouldBe` False

  describe "GitHub webhook payloads" do
    it "parses a pull request event" do
      eitherDecode "{\"action\":\"enqueued\",\"pull_request\":{\"number\":5},\"repository\":{\"name\":\"Bar\",\"owner\":{\"login\":\"Foo\"}}}"
        `shouldBe` Right GithubWebhookPayload {repository = Just ("foo", "bar"), pullRequestNumber = Just 5, action = Just "enqueued"}
    it "parses a ping" do
      eitherDecode "{\"zen\":\"hi\",\"repository\":{\"name\":\"bar\",\"owner\":{\"login\":\"foo\"}}}"
        `shouldBe` Right GithubWebhookPayload {repository = Just ("foo", "bar"), pullRequestNumber = Nothing, action = Nothing}

  describe "Which webhooks refresh a PR" do
    it "refreshes on state changes" do
      changesPrState "pull_request" (Just "closed") `shouldBe` True
      changesPrState "pull_request" (Just "enqueued") `shouldBe` True
      changesPrState "pull_request_review" (Just "dismissed") `shouldBe` True
    it "ignores other actions and events" do
      changesPrState "pull_request" (Just "labeled") `shouldBe` False
      changesPrState "pull_request_review" (Just "edited") `shouldBe` False
      changesPrState "ping" Nothing `shouldBe` False
