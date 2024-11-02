module Slacklinker.Extract.FreeTextSpec (spec) where

import Slacklinker.Extract.FreeText (extractUrls)
import TestImport

spec :: Spec
spec = describe "Free Text URL Parsing" do
  describe "Parses" do
    it "just the url" do
      let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
    it "multiple URLs" do
      let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249 https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249", "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"]
    it "URLs with text before it" do
      let urls = extractUrls "myteam" "this is a fake pr with a link https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
    it "URLs with text after it" do
      let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249 some text here"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
    it "many URLs with text inline" do
      let urls = extractUrls "myteam" "some other text https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249 some https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250 text here"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249", "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"]
    it "URLs with newlines and formatting" do
      let urls = extractUrls "myteam" "this is a fake pr with a link\n\n<https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249|https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249>\n and some content"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249", "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
    it "thread URL" do
      let urls = extractUrls "myteam" "this is a fake pr with a link https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249?thread_ts=1730446100.644549"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249?thread_ts=1730446100.644549&cid=C07KTH1T4CQ"]
    it "thread URLs with extra query params" do
      let urls = extractUrls "myteam" "this is a fake pr with a link https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249?abc=def&thread_ts=1730446100.644549&cid=C07KTH1T4CQ"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249?thread_ts=1730446100.644549&cid=C07KTH1T4CQ"]
    it "thread URLs with query params and then some other stuff" do
      let urls = extractUrls "myteam" "this is a fake pr with a link https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249?thread_ts=1730446100.644549&cid=C07KTH1T4CQ and also this\nhttps://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249?thread_ts=1730446100.644549&cid=C07KTH1T4CQ", "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"]
    it "slack URLs with unrelated params" do
      let urls = extractUrls "myteam" "this is a fake pr with a link https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249?foo=bar"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
    it "a url in the middle of a word" do
      -- this might be controversial, but getting the parser to throw these out is a bit difficult
      let urls = extractUrls "myteam" "there is no space after the word link: linkhttps://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
    it "multiple URLs without spaces between them" do
      let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"]
    it "URLs with mixed case" do
      let urls = extractUrls "myteam" "HtTpS://myteaM.slack.com/Archives/C07KTH1T4CQ/p1730339894629249"
      urls `shouldBe` []
    -- we don't have actual markdown parsing
    -- but we fake it with trimEndings to support common use cases
    it "markdown URLs" do
      let urls = extractUrls "myteam" "[thisisaurl](https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249) hey"
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
    it "URLs with period punctuation ending" do
      let urls = extractUrls "myteam" "check out my slack message: https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249."
      urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
  describe "Doesnt Parse" do
    it "a url without protocol" do
      let urls = extractUrls "myteam" "myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"
      urls `shouldBe` []
    it "a url for a different team" do
      let urls = extractUrls "notmyteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"
      urls `shouldBe` []
    it "a non-slack url" do
      let urls = extractUrls "myteam" "https://myteam.mercury.com/archives/C07KTH1T4CQ/p1730339894629249"
      urls `shouldBe` []
    it "a slack url not for a message" do
      let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C05JCSKNE2G https://app.slack.com/huddle/T05JP2XHGGZ/C05JCSKNE2G https://myteam.slack.com/something/C07KTH1T4CQ/p1730339894629249"
      urls `shouldBe` []
    it "a message with  no URLs" do
      let urls = extractUrls "myteam" "hello this is a message"
      urls `shouldBe` []
    it "URLs with encoded spaces or special characters" do
      let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249%20"
      urls `shouldBe` []
    it "partially matching URLs" do
      let urls = extractUrls "myteam" "https://myteam.slack.com/archives/"
      urls `shouldBe` []
