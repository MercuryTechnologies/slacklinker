{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

-- | Implements authentication setup for Linear for a workspace.
module Slacklinker.Linear.OAuth (makeAuthorizationURI, getNonceWorkspaceAndInvalidate, exchangeCodeForToken) where

import Control.Monad.Trans.Except (runExceptT)
import Crypto.Random.Entropy (getEntropy)
import Data.ByteString.Base64.URL qualified as B64
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Time (addUTCTime, secondsToNominalDiffTime)
import Database.Persist qualified as P
import Network.HTTP.Client (Manager)
import Network.OAuth.OAuth2 (ExchangeToken (..), OAuth2Token)
import Network.OAuth2.Experiment qualified as HOAuth2
import Slacklinker.App (HasApp (..), runDB)
import Slacklinker.Exceptions (LinearOAuth2Error (..))
import Slacklinker.Linear.Types (LinearClientId (..), LinearClientSecret (..), LinearCreds (..))
import Slacklinker.Models (EntityField (..), LinearNonce (..), Unique (..), WorkspaceId)
import Slacklinker.Prelude
import URI.ByteString (Absolute, Authority (..), Host (..), Scheme (..), URIRef (..))
import URI.ByteString.QQ

-- We use Network.OAuth2.Experiment since it actually does state parameters
-- correctly.
data LinearIdP = LinearIdP deriving stock (Show, Eq)

linearIdP :: HOAuth2.Idp LinearIdP
linearIdP =
  HOAuth2.Idp
    { idpUserInfoEndpoint = error "not using linear user info endpoint"
    , idpAuthorizeEndpoint = [uri|https://linear.app/oauth/authorize|]
    , idpTokenEndpoint = [uri|https://api.linear.app/oauth/token|]
    , idpDeviceAuthorizationEndpoint = error "not using linear device authorization"
    }

desiredScopes :: Set HOAuth2.Scope
desiredScopes =
  Set.fromList
    [ -- Required to get any metadata about the workspace at all e.g. team IDs.
      "read"
    , -- For adding attachments to tickets.
      "write"
    ]

deleteExpiredNonces :: (MonadIO m) => UTCTime -> SqlPersistT m ()
deleteExpiredNonces now = do
  P.deleteWhere [LinearNonceExpiresAt P.<. now]

{- | Makes a nonce and saves it in the database; deletes all expired ones
first.

This is tightly bound to a workspace ID, which avoids potential CSRF issues
(since you can't produce a @state@ parameter without having an authenticated
session of the appropriate tenant).
-}
makeStateParamAndSave :: (MonadIO m, HasApp m) => WorkspaceId -> m Text
makeStateParamAndSave workspaceId = do
  -- 48 bytes of randomness ought to be enough for anyone; not brute forceable
  -- in the lifetime of the known universe. Arbitrarily chosen to produce 64
  -- byte long state parameters, in case anyone is limiting the length of
  -- those.
  entropy <- liftIO $ getEntropy 48

  -- This is the same length as Slack uses for this, but is otherwise
  -- arbitrary.
  let tenMinutes = secondsToNominalDiffTime $ 10 * 60

  now <- liftIO getCurrentTime
  runDB $ do
    deleteExpiredNonces now
    P.insert_
      $ LinearNonce
        { nonceValue = entropy
        , expiresAt = tenMinutes `addUTCTime` now
        , workspaceId
        }
  pure . decodeUtf8 . B64.encode $ entropy

makeAuthorizationCodeApp :: (MonadIO m, HasApp m) => Text -> WorkspaceId -> LinearCreds -> m HOAuth2.AuthorizationCodeApplication
makeAuthorizationCodeApp httpHost workspaceId creds = do
  state <- makeStateParamAndSave workspaceId

  pure
    HOAuth2.AuthorizationCodeApplication
      { acName = "slacklinker linear"
      , acClientId = HOAuth2.ClientId . cs $ creds.linearClientId.unLinearClientId
      , acClientSecret = HOAuth2.ClientSecret . cs $ creds.linearClientSecret.unLinearClientSecret
      , acScope = desiredScopes
      , acAuthorizeState = HOAuth2.AuthorizeState $ cs state
      , -- FIXME(jadel): this probably should be using Servant.Links but it would
        -- cause a circular dependency in the current module structuring.
        -- I think that we would have to define APIs in separate files and then
        -- define the implementations thereof in different files.
        acRedirectUri =
          URI
            { uriScheme = Scheme "https"
            , uriAuthority = Just $ Authority {authorityUserInfo = Nothing, authorityPort = Nothing, authorityHost = Host $ encodeUtf8 httpHost}
            , uriPath = "/linear/oauth_redirect"
            , uriQuery = mempty
            , uriFragment = Nothing
            }
      , -- See: https://developers.linear.app/docs/oauth/authentication
        acAuthorizeRequestExtraParams =
          Map.fromList
            [ -- Application should act as itself, not as a user
              ("actor", "application")
            ]
      , -- XXX(jadel): This is not actually sent in the form as specified in the
        -- Linear API spec due to a bug in hoauth2. It is instead sent in the
        -- Authorization header, and it still works, apparently?
        -- Nevertheless I filed a bug: https://github.com/freizl/hoauth2/issues/252
        acTokenRequestAuthenticationMethod = HOAuth2.ClientSecretPost
      }

-- | Gets the workspace of the given @state@ nonce and invalidates the nonce.
getNonceWorkspaceAndInvalidate :: (MonadIO m) => ByteString -> SqlPersistT m (Maybe WorkspaceId)
getNonceWorkspaceAndInvalidate nonce = do
  now <- liftIO getCurrentTime
  deleteExpiredNonces now

  -- Delete the nonce itself so it cannot be reused
  mEntity <- P.getBy (UniqueLinearNonceValue nonce)
  for_ (entityKey <$> mEntity) P.delete

  pure $ (.workspaceId) . entityVal <$> mEntity

exchangeCodeForToken :: (HasApp m, MonadIO m) => Text -> WorkspaceId -> LinearCreds -> Manager -> Text -> m OAuth2Token
exchangeCodeForToken httpHost workspaceId creds manager code = do
  idpApp <- HOAuth2.IdpApplication linearIdP <$> makeAuthorizationCodeApp httpHost workspaceId creds
  tokenResp <- runExceptT $ HOAuth2.conduitTokenRequest idpApp manager (ExchangeToken code)
  fromEither . mapLeft (LinearOAuth2Error . tshow) $ tokenResp

-- | Makes the URI to send the user to for authenticating to a new Linear workspace.
makeAuthorizationURI :: (HasApp m, MonadIO m) => Text -> WorkspaceId -> LinearCreds -> m (URIRef Absolute)
makeAuthorizationURI httpHost workspaceId creds =
  HOAuth2.mkAuthorizationRequest . HOAuth2.IdpApplication linearIdP <$> makeAuthorizationCodeApp httpHost workspaceId creds
