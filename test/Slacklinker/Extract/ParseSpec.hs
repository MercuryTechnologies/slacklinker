module Slacklinker.Extract.ParseSpec (spec) where

import Slacklinker.Extract.Parse (extractUrls)
import TestImport

spec :: Spec
spec = describe "Free Text URL Parsing" do
  it "Parses just the url" do
    let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"
    urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
  it "Parses multiple URLs" do
    let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249 https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"
    urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249", "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"]
  it "Parses URLs with text before it" do
    let urls = extractUrls "myteam" "this is a fake pr with a link https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"
    urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
  it "Parses URLs with text after it" do
    let urls = extractUrls "myteam" "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249 some text here"
    urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
  it "Parses many URLs with text inline" do
    let urls = extractUrls "myteam" "some other text https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249 some https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250 text here"
    urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249", "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"]
  it "Parses URLs with newlines and formatting" do
    let urls = extractUrls "myteam" "this is a fake pr with a link\n\n<https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249|https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249>\n and some content"
    urls `shouldBe` ["https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249", "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"]
