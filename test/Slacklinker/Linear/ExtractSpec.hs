module Slacklinker.Linear.ExtractSpec where

import Slacklinker.Linear.Extract (LinearTicketId (..), extractTicketIds)
import TestImport

-- Helper function to run test cases
runTestCases :: [(String, (Text, [LinearTicketId]))] -> Spec
runTestCases =
  mapM_
    ( \(testName, (input, expected)) ->
        it testName $ extractTicketIds input `shouldBe` expected
    )

spec :: Spec
spec = describe "Extracting Linear ticket IDs" do
  runTestCases
    [ ("start of input", ("TICK-1234", [LinearTicketId "TICK" 1234]))
    , ("end of input", (", TICK-1234", [LinearTicketId "TICK" 1234]))
    , ("middle of input", ("meow, TICK-1234, indeed, mrrp", [LinearTicketId "TICK" 1234]))
    , ("in parens", ("blah blah (TICK-1234)", [LinearTicketId "TICK" 1234]))
    , ("with one space between", ("TICK-1234 TICK-5678", [LinearTicketId "TICK" 1234, LinearTicketId "TICK" 5678]))
    , ("in URLs", ("https://linear.app/meowmeow/issue/MER-3/connect-to-slack", [LinearTicketId "MER" 3]))
    ]
