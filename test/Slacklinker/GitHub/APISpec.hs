module Slacklinker.GitHub.APISpec where

import Data.Aeson (Value, encode, object, (.=))
import Data.Either (isLeft)
import Slacklinker.GitHub.API (prStateFromHttp)
import Slacklinker.GitHub.Types
import TestImport

response :: Text -> Bool -> Bool -> Maybe Text -> [Text] -> Value
response state isDraft isInMergeQueue reviewDecision reviews =
  object
    [ "data"
        .= object
          [ "repository"
              .= object
                [ "pullRequest"
                    .= object
                      [ "state" .= state
                      , "isDraft" .= isDraft
                      , "isInMergeQueue" .= isInMergeQueue
                      , "reviewDecision" .= reviewDecision
                      , "latestOpinionatedReviews" .= object ["nodes" .= [object ["state" .= r] | r <- reviews]]
                      ]
                ]
          ]
    ]

prStateFromResponse :: Value -> Either Text PrState
prStateFromResponse = prStateFromHttp 200 . encode

spec :: Spec
spec = describe "PR state from GraphQL" do
  let st s r = Right $ PrState s r
  it "open" $ prStateFromResponse (response "OPEN" False False Nothing []) `shouldBe` st PrOpened ReviewNone
  it "draft" $ prStateFromResponse (response "OPEN" True False Nothing []) `shouldBe` st PrDraft ReviewNone
  it "queued" $ prStateFromResponse (response "OPEN" False True (Just "APPROVED") []) `shouldBe` st PrQueued ReviewApproved
  it "merged" $ prStateFromResponse (response "MERGED" False False (Just "APPROVED") []) `shouldBe` st PrMerged ReviewApproved
  it "closed" $ prStateFromResponse (response "CLOSED" False False (Just "CHANGES_REQUESTED") []) `shouldBe` st PrClosed ReviewChangesRequested
  it "review required" $ prStateFromResponse (response "OPEN" False False (Just "REVIEW_REQUIRED") ["APPROVED"]) `shouldBe` st PrOpened ReviewNone
  it "falls back to reviews without a decision" do
    prStateFromResponse (response "OPEN" False False Nothing ["APPROVED"]) `shouldBe` st PrOpened ReviewApproved
    prStateFromResponse (response "OPEN" False False Nothing ["APPROVED", "CHANGES_REQUESTED"]) `shouldBe` st PrOpened ReviewChangesRequested
  it "fails when not found" do
    isLeft (prStateFromResponse (object ["data" .= object ["repository" .= (Nothing :: Maybe Value)]])) `shouldBe` True
  it "reports HTTP errors with GitHub's message" do
    prStateFromHttp 401 "{\"message\":\"Bad credentials\"}" `shouldBe` Left "GitHub returned HTTP 401: Bad credentials"
    prStateFromHttp 502 "bad gateway" `shouldBe` Left "GitHub returned HTTP 502: bad gateway"
