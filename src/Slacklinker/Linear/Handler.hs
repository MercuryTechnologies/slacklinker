{-# LANGUAGE TypeOperators #-}

module Slacklinker.Linear.Handler (Api, linearH) where

import Data.ByteString.Base64.URL qualified as B64
import Database.Persist qualified as P
import Network.OAuth.OAuth2 (AccessToken (..), OAuth2Token (..))
import Servant.API (Get, PlainText, QueryParam', Required, (:>))
import Slacklinker.App (App (..), AppConfig (..), AppM, HasApp (..), runDB)
import Slacklinker.Exceptions (BadBase64 (..), BadNonce (..), LinearDisabled (..))
import Slacklinker.Import (orThrow)
import Slacklinker.Linear.OAuth (exchangeCodeForToken, getNonceWorkspaceAndInvalidate)
import Slacklinker.Linear.Organization (LinearOrganizationMetadata (..), linearOrganizationMetadata)
import Slacklinker.Linear.Session (updateAuthSession)
import Slacklinker.Linear.Types (LinearBearerToken (..))
import Slacklinker.Models (EntityField (..), LinearOrganization (..), Unique (..))
import Slacklinker.Prelude

type Api =
  "oauth_redirect"
    :> QueryParam' '[Required] "code" Text
    :> QueryParam' '[Required] "state" Text
    :> Get '[PlainText] Text

decodeBase64Throw :: (MonadIO m) => Text -> m ByteString
decodeBase64Throw = fromEither . mapLeft BadBase64 . B64.decode . encodeUtf8

getOauthRedirectR :: Text -> Text -> AppM Text
getOauthRedirectR code state = do
  httpHost <- getsApp (.config.slacklinkerHost) >>= (`orThrow` LinearDisabled)
  linearCreds <- getsApp (.config.linearCreds) >>= (`orThrow` LinearDisabled)
  manager <- getsApp (.manager)

  workspaceId_ <- runDB $ getNonceWorkspaceAndInvalidate =<< decodeBase64Throw state
  workspaceId <- workspaceId_ `orThrow` BadNonce
  now <- liftIO getCurrentTime

  tokenResponse <- exchangeCodeForToken httpHost workspaceId linearCreds manager code
  orgMeta <- linearOrganizationMetadata (LinearBearerToken tokenResponse.accessToken.atoken)

  runDB do
    Entity linearOrgId _ <-
      P.upsertBy
        (UniqueLinearOrganization workspaceId orgMeta.id)
        (LinearOrganization {workspaceId = workspaceId, linearId = orgMeta.id, urlKey = orgMeta.urlKey, displayName = orgMeta.name})
        [LinearOrganizationUrlKey P.=. orgMeta.urlKey, LinearOrganizationDisplayName P.=. orgMeta.name]
    updateAuthSession now linearOrgId tokenResponse

  pure "Authorized!"

-- | All Linear endpoint handlers
linearH :: Text -> Text -> AppM Text
linearH = getOauthRedirectR
