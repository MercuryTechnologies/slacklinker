-- | Getting a usable auth token from Linear.
module Slacklinker.Linear.Session where

import Data.Time.Clock (addUTCTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Database.Esqueleto.Experimental (Value (..))
import Database.Persist qualified as P
import Network.OAuth.OAuth2 (AccessToken (..), OAuth2Token (..), RefreshToken (..))
import Slacklinker.App (App (..), AppConfig (..), HasApp (..), runDB)
import Slacklinker.Exceptions (LinearDisabled (..), LinearNotAuthenticated (..), LinearRaced (..))
import Slacklinker.Import
import Slacklinker.Linear.DB (linearAuthSessionForWorkspace, lockLinearAuthSession)
import Slacklinker.Linear.OAuth (refreshSession)
import Slacklinker.Linear.Types (LinearBearerToken (..), LinearRefreshToken (..))
import Slacklinker.Models (EntityField (..), LinearAPIAuthSession (..), LinearOrganizationId, Unique (..), WorkspaceId)

{- | Updates the authentication session for a given Linear org with a new
access/refresh token pair from a token request.
-}
updateAuthSession :: (MonadIO m) => UTCTime -> LinearOrganizationId -> OAuth2Token -> SqlPersistT m ()
updateAuthSession now linearOrgId tokenResponse = do
  let expiresAt = (`addUTCTime` now) . secondsToNominalDiffTime . fromIntegral <$> tokenResponse.expiresIn
      token = LinearBearerToken tokenResponse.accessToken.atoken
      refreshToken = LinearRefreshToken . (.rtoken) <$> tokenResponse.refreshToken

  void
    $ P.upsertBy
      (UniqueLinearAPIAuthSession linearOrgId)
      (LinearAPIAuthSession {linearOrganizationId = linearOrgId, token, expiresAt, refreshToken})
      [LinearAPIAuthSessionToken P.=. token, LinearAPIAuthSessionExpiresAt P.=. expiresAt, LinearAPIAuthSessionRefreshToken P.=. refreshToken]

realRefreshToken :: (MonadIO m, HasApp m) => WorkspaceId -> LinearRefreshToken -> m OAuth2Token
realRefreshToken workspaceId refreshToken = do
  mgr <- getsApp (.manager)
  httpHost <- getsApp (.config.slacklinkerHost) >>= (`orThrow` LinearDisabled)
  linearCreds <- getsApp (.config.linearCreds) >>= (`orThrow` LinearDisabled)
  refreshSession httpHost workspaceId linearCreds mgr refreshToken

{- | Gets a (possibly-new) token for Linear.

We do sorta evil stuff with DB locking and "http in transaction" to avoid
races (since refresh tokens cannot be reused).
-}
getToken' ::
  (MonadUnliftIO m, HasApp m) =>
  (WorkspaceId -> LinearRefreshToken -> m OAuth2Token) ->
  WorkspaceId ->
  m (LinearOrganizationId, LinearBearerToken)
getToken' refresher workspaceId = do
  withRunInIO \runInIO -> runInIO $ runDB do
    (_, Entity sessionId_ _) <-
      linearAuthSessionForWorkspace workspaceId
        >>= (`orThrow` LinearNotAuthenticated)
    lockLinearAuthSession sessionId_

    -- Once we lock the row, we have to re-fetch it to make sure we definitely
    -- got the copy from the last time it was unlocked.
    (Value linearOrgId, Entity sessionId session) <-
      linearAuthSessionForWorkspace workspaceId
        >>= (`orThrow` LinearNotAuthenticated)

    unless (sessionId_ == sessionId) $ throwIO LinearRaced

    now <- liftIO getCurrentTime
    let expiry = fromMaybe (posixSecondsToUTCTime 0) session.expiresAt
    if (now > (-thirtyMinutes) `addUTCTime` expiry)
      then do
        -- The failure case here is actually impossible: old Linear sessions have
        -- 10y in the future expiry.
        refreshToken <- session.refreshToken `orThrow` LinearNotAuthenticated
        newToken <- liftIO $ runInIO $ refresher workspaceId refreshToken
        updateAuthSession now linearOrgId newToken

        pure (linearOrgId, LinearBearerToken newToken.accessToken.atoken)
      else pure (linearOrgId, session.token)
  where
    -- Arbitrarily chosen threshold: session tokens live for 24h.
    thirtyMinutes = secondsToNominalDiffTime $ 30 * 60

-- | Get a fresh token without using a stub.
getToken :: (MonadUnliftIO m, HasApp m) => WorkspaceId -> m (LinearOrganizationId, LinearBearerToken)
getToken = getToken' realRefreshToken
