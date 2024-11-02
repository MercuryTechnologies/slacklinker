module Slacklinker.Extract.FreeTextSpec (spec) where

import Slacklinker.Extract.FreeText (extractUrls)
import TestImport

type TestCase = (Text, [Text]) -- (input text, expected URLs)

validSlackUrl :: Text
validSlackUrl = "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249"

validSlackUrl2 :: Text
validSlackUrl2 = "https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629250"

threadUrl :: Text
threadUrl = validSlackUrl <> "?thread_ts=1730446100.644549&cid=C07KTH1T4CQ"

-- Helper function to run test cases
runTestCases :: String -> [(String, TestCase)] -> Spec
runTestCases description cases =
  describe description
    $ mapM_
      ( \(testName, (input, expected)) ->
          it testName $ extractUrls "myteam" input `shouldBe` expected
      )
      cases

spec :: Spec
spec = describe "Free Text URL Parsing" $ do
  describe "Basic URL parsing" $ do
    let basicCases =
          [ ("single URL", (validSlackUrl, [validSlackUrl]))
          , ("multiple URLs", (validSlackUrl <> " " <> validSlackUrl2, [validSlackUrl, validSlackUrl2]))
          ]
    runTestCases "should parse" basicCases

  describe "URLs with surrounding text" $ do
    let textCases =
          [ ("text before URL", ("here is a link " <> validSlackUrl, [validSlackUrl]))
          , ("text after URL", (validSlackUrl <> " some text here", [validSlackUrl]))
          , ("text before and after", ("check this " <> validSlackUrl <> " link out", [validSlackUrl]))
          , ("multiple URLs with text", ("first " <> validSlackUrl <> " middle " <> validSlackUrl2 <> " end", [validSlackUrl, validSlackUrl2]))
          ]
    runTestCases "should parse" textCases

  describe "Special formatting cases" $ do
    let formattingCases =
          [ ("newlines and Slack formatting", ("message with link\n\n<" <> validSlackUrl <> "|" <> validSlackUrl <> ">\nmore content", [validSlackUrl, validSlackUrl]))
          , -- we don't have actual markdown parsing
            -- but we fake it with trimEndings to support common use cases
            ("markdown link", ("[link](" <> validSlackUrl <> ")", [validSlackUrl]))
          , ("URL with trailing period", (validSlackUrl <> ".", [validSlackUrl]))
          ]
    runTestCases "should handle" formattingCases

  describe "Thread URLs" $ do
    let threadCases =
          [ ("basic thread URL", (threadUrl, [threadUrl]))
          , ("thread URL with extra params", (validSlackUrl <> "?abc=def&thread_ts=1730446100.644549&cid=C07KTH1T4CQ", [threadUrl]))
          , ("multiple URLs with thread", (threadUrl <> " and " <> validSlackUrl2, [threadUrl, validSlackUrl2]))
          ]
    runTestCases "should parse" threadCases

  describe "Invalid cases" $ do
    let invalidCases =
          [ ("URL without protocol", ("myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249", []))
          , ("different team URL", ("https://otherteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249", []))
          , ("non-Slack URL", ("https://example.com/archives/123", []))
          , ("non-message Slack URL", ("https://myteam.slack.com/archives/C05JCSKNE2G", []))
          , ("no URLs", ("just some regular text", []))
          , ("empty text", ("", []))
          , ("URL with encoded spaces", ("https://myteam.slack.com/archives%20/C07KTH1T4CQ/p1730339894629249", []))
          , ("URL with encoded spaces at end", ("https://myteam.slack.com/archives/C07KTH1T4CQ/p1730339894629249%20", []))
          , ("incomplete URL", ("https://myteam.slack.com/archives/", []))
          , ("mixed case URL", ("HtTpS://myteaM.slack.com/Archives/C07KTH1T4CQ/p1730339894629249", []))
          ]
    runTestCases "should not parse" invalidCases

  describe "Edge cases" $ do
    let edgeCases =
          [ ("URL without spaces", (validSlackUrl <> validSlackUrl2, [validSlackUrl2]))
          , -- this might be controversial, but getting the parser to throw these out is a bit difficult
            ("URL in middle of word", ("no space after link" <> validSlackUrl, [validSlackUrl]))
          ]
    runTestCases "should handle" edgeCases
