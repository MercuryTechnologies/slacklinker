module Slacklinker.GitHub.TypesSpec where

import Slacklinker.GitHub.Types
import TestImport

spec :: Spec
spec = describe "Shown review" do
  it "shows the review while open" do
    shownReview (PrState PrOpened ReviewApproved) `shouldBe` ReviewApproved
    shownReview (PrState PrQueued ReviewApproved) `shouldBe` ReviewApproved
    shownReview (PrState PrDraft ReviewChangesRequested) `shouldBe` ReviewChangesRequested
  it "hides the review once merged or closed" do
    shownReview (PrState PrMerged ReviewApproved) `shouldBe` ReviewNone
    shownReview (PrState PrClosed ReviewChangesRequested) `shouldBe` ReviewNone
