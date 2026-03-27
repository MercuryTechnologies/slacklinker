module Slacklinker.Linear.SessionSpec (spec) where

import Data.Time.Clock (addUTCTime, secondsToNominalDiffTime)
import Database.Persist (insert, insert_)
import Network.OAuth.OAuth2 (AccessToken (..), OAuth2Token (..), RefreshToken (..))
import Slacklinker.App (runAppM, runDB)
import Slacklinker.Exceptions (LinearNotAuthenticated (..))
import Slacklinker.Linear.Session (getToken')
import Slacklinker.Linear.Types (LinearBearerToken (..), LinearRefreshToken (..))
import Slacklinker.Models
import TestApp (withApp)
import TestImport
import TestUtils (createWorkspace)

setupLinearSession :: (MonadIO m) => WorkspaceId -> Maybe UTCTime -> Maybe LinearRefreshToken -> Text -> SqlPersistT m ()
setupLinearSession wsId expiresAt refreshToken token = do
  orgId <-
    insert
      LinearOrganization
        { workspaceId = wsId
        , linearId = "org-123"
        , urlKey = "testorg"
        , displayName = "Test Org"
        }
  insert_
    LinearAPIAuthSession
      { linearOrganizationId = orgId
      , token = LinearBearerToken token
      , expiresAt
      , refreshToken
      }

cannedOAuth2Token :: OAuth2Token
cannedOAuth2Token =
  OAuth2Token
    { accessToken = AccessToken "new-access-token"
    , refreshToken = Just (RefreshToken "new-refresh-token")
    , expiresIn = Just 86400
    , tokenType = Nothing
    , idToken = Nothing
    , scope = Nothing
    , rawResponse = error "not used"
    }

spec :: Spec
spec = do
  withApp $ describe "getToken'" do
    it "returns existing token when not expired" \app -> runAppM app do
      (wsId, _) <- createWorkspace
      now <- liftIO getCurrentTime
      let futureExpiry = addUTCTime (secondsToNominalDiffTime 3600) now
      runDB $ setupLinearSession wsId (Just futureExpiry) (Just $ LinearRefreshToken "refresh") "existing-token"

      refresherCalled <- liftIO $ newIORef False
      let fakeRefresher _ _ = do
            liftIO $ writeIORef refresherCalled True
            pure cannedOAuth2Token

      (_, result) <- getToken' fakeRefresher wsId
      liftIO do
        result `shouldBe` LinearBearerToken "existing-token"
        readIORef refresherCalled >>= (`shouldBe` False)

    it "refreshes when token is near expiry" \app -> runAppM app do
      (wsId, _) <- createWorkspace
      now <- liftIO getCurrentTime
      -- 10 minutes from now — within the 30-minute refresh threshold
      let nearExpiry = addUTCTime (secondsToNominalDiffTime 600) now
      runDB $ setupLinearSession wsId (Just nearExpiry) (Just $ LinearRefreshToken "old-refresh") "old-token"

      refresherCalled <- liftIO $ newIORef False
      let fakeRefresher _ _ = do
            liftIO $ writeIORef refresherCalled True
            pure cannedOAuth2Token

      (_, result) <- getToken' fakeRefresher wsId
      liftIO do
        result `shouldBe` LinearBearerToken "new-access-token"
        readIORef refresherCalled >>= (`shouldBe` True)

    it "throws when expired and no refresh token" \app -> runAppM app do
      (wsId, _) <- createWorkspace
      now <- liftIO getCurrentTime
      let pastExpiry = addUTCTime (secondsToNominalDiffTime (-3600)) now
      runDB $ setupLinearSession wsId (Just pastExpiry) Nothing "old-token"

      let fakeRefresher _ _ = pure cannedOAuth2Token

      withRunInIO \runInIO ->
        runInIO (getToken' fakeRefresher wsId)
          `shouldThrow` (\LinearNotAuthenticated -> True)

    it "throws when no session exists" \app -> runAppM app do
      (wsId, _) <- createWorkspace

      let fakeRefresher _ _ = pure cannedOAuth2Token

      withRunInIO \runInIO ->
        runInIO (getToken' fakeRefresher wsId)
          `shouldThrow` (\LinearNotAuthenticated -> True)
