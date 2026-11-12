module Slacklinker.GitHub.APISpec (spec) where

import Data.Aeson (encode, object, (.=))
import Data.Either (isLeft)
import Slacklinker.GitHub.API (prStateFromHttp)
import Slacklinker.GitHub.Types
import TestImport

data Pr = Pr
  { state :: Text
  , isDraft :: Bool
  , isInMergeQueue :: Bool
  , reviewDecision :: Maybe Text
  , reviews :: [Text]
  }

openPr :: Pr
openPr = Pr {state = "OPEN", isDraft = False, isInMergeQueue = False, reviewDecision = Nothing, reviews = []}

toResponse :: Pr -> LByteString
toResponse pr =
  encode
    $ object
      [ "data"
          .= object
            [ "repository"
                .= object
                  [ "pullRequest"
                      .= object
                        [ "state" .= pr.state
                        , "isDraft" .= pr.isDraft
                        , "isInMergeQueue" .= pr.isInMergeQueue
                        , "reviewDecision" .= pr.reviewDecision
                        , "latestOpinionatedReviews" .= object ["nodes" .= [object ["state" .= r] | r <- pr.reviews]]
                        ]
                  ]
            ]
      ]

spec :: Spec
spec = describe "PR state from GraphQL" do
  for_
    [ ("open", openPr, PrState PrOpened ReviewNone)
    , ("draft", openPr {isDraft = True}, PrState PrDraft ReviewNone)
    , ("in merge queue", openPr {isInMergeQueue = True, reviewDecision = Just "APPROVED"}, PrState PrQueued ReviewApproved)
    , ("merged", openPr {state = "MERGED", reviewDecision = Just "APPROVED"}, PrState PrMerged ReviewApproved)
    , ("closed", openPr {state = "CLOSED", reviewDecision = Just "CHANGES_REQUESTED"}, PrState PrClosed ReviewChangesRequested)
    , ("review required ignores reviews", openPr {reviewDecision = Just "REVIEW_REQUIRED", reviews = ["APPROVED"]}, PrState PrOpened ReviewNone)
    , ("approved review without a decision", openPr {reviews = ["APPROVED"]}, PrState PrOpened ReviewApproved)
    , ("changes requested wins without a decision", openPr {reviews = ["APPROVED", "CHANGES_REQUESTED"]}, PrState PrOpened ReviewChangesRequested)
    ]
    \(name, pr, expected) -> it name $ prStateFromHttp 200 (toResponse pr) `shouldBe` Right expected

  it "fails when the PR is missing" do
    prStateFromHttp 200 "{\"data\":{\"repository\":null}}" `shouldSatisfy` isLeft

  it "reports HTTP errors with GitHub's message" do
    prStateFromHttp 401 "{\"message\":\"Bad credentials\"}" `shouldBe` Left "GitHub returned HTTP 401: Bad credentials"
    prStateFromHttp 502 "bad gateway" `shouldBe` Left "GitHub returned HTTP 502: bad gateway"
