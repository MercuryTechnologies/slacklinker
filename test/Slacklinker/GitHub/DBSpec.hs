module Slacklinker.GitHub.DBSpec where

import Database.Persist
import Slacklinker.App (runAppM, runDB)
import Slacklinker.GitHub.DB (knownGithubRepos, linksForPullRequest, registerGithubRepo)
import Slacklinker.GitHub.Types
import Slacklinker.Models
import TestApp
import TestImport
import TestUtils (createWorkspace)
import Web.Slack.Conversation (ConversationId (..))

spec :: Spec
spec = withApp $ describe "GitHub PR tracking DB" do
  it "round-trips repos, PRs and links" \app -> runAppM app do
    (wsId, _) <- createWorkspace
    (known, links) <- runDB do
      repoId <- insert GithubRepo {owner = "foo", name = "bar"}
      prId <- insert GithubPullRequest {repoId, number = 7}
      channelId <- insert JoinedChannel {workspaceId = wsId, channelId = ConversationId "C1", name = Nothing, trackGithubPrs = True}
      insert_ LinkedGithubPullRequest {pullRequestId = prId, joinedChannelId = channelId, messageTs = "1.2", appliedStatus = Just PrDraft, appliedReview = ReviewApproved}
      (,) <$> knownGithubRepos [PrRef "foo" "bar" 7, PrRef "foo" "baz" 1] <*> linksForPullRequest prId
    liftIO do
      toList known `shouldBe` [("foo", "bar")]
      [(Entity _ l, Entity _ c, _)] <- pure links
      (l.appliedStatus, l.appliedReview, c.channelId) `shouldBe` (Just PrDraft, ReviewApproved, ConversationId "C1")

  it "registers a repo idempotently" \app -> runAppM app do
    (a, b) <- runDB $ (,) <$> registerGithubRepo "foo" "bar" <*> registerGithubRepo "foo" "bar"
    liftIO $ a `shouldBe` b
