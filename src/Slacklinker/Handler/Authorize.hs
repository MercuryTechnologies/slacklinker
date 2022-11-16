-- | Deals with all the OAuth stuff
module Slacklinker.Handler.Authorize where

import Crypto.Random.Entropy (getEntropy)
import Data.ByteString.Base64.URL qualified as B64
import Data.Time
import Database.Persist
import Slacklinker.App (App (..), AppConfig (..), AppM, getApp, runDB)
import Slacklinker.Exceptions
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender (runSlack)
import Slacklinker.Settings
import Slacklinker.Slack.OAuth
import Slacklinker.Slack.TeamInfo
import URI.ByteString

desiredScopes :: ByteString
desiredScopes =
  intercalate
    ","
    [ -- read chat messages
      "channels:history"
    , -- join itself to channels
      "channels:join"
    , -- find public channels
      "channels:read"
    , -- send messages
      "chat:write"
    , -- read the team URL
      "team:read"
    , -- take commands via IM
      "im:history"
    , -- know which IMs it's in (?!)
      "im:read"
    ]

slackRedirectUriRef :: ByteString -> ByteString -> URIRef Absolute
slackRedirectUriRef clientId state =
  URI
    { uriScheme = Scheme "https"
    , uriAuthority = Just Authority {authorityHost = Host "slack.com", authorityUserInfo = Nothing, authorityPort = Nothing}
    , uriPath = "/oauth/v2/authorize"
    , uriQuery =
        Query
          [ ("scope", desiredScopes)
          , ("client_id", clientId)
          , ("state", state)
          ]
    , uriFragment = Nothing
    }

deleteExpiredNonces :: UTCTime -> SqlPersistT IO ()
deleteExpiredNonces now = do
  deleteWhere [NonceExpiresAt <. now]

getAndSaveState :: AppM ByteString
getAndSaveState = do
  entropy <- liftIO $ getEntropy 48
  now <- liftIO getCurrentTime
  runDB $ do
    deleteExpiredNonces now
    insert_ $ Nonce {nonceValue = entropy, expiresAt = (secondsToNominalDiffTime $ 10 * 60) `addUTCTime` now}
  pure . B64.encode $ entropy

getAuthorizeR :: AppM Text
getAuthorizeR = do
  ~(SettingAllowRegistration allowRegistration) <- runDB $ getSetting @'AllowRegistration

  -- FIXME(jadel): this doesn't let us get new scopes for existing workspaces
  -- when registration is disabled, but that's probably fine.
  unless allowRegistration $ throwIO RegistrationDisabled

  clientId <- cs . (.config.slackClientId) <$> getApp

  state <- getAndSaveState

  let redirectUrl = serializeURIRef' $ slackRedirectUriRef clientId state
  throwIO $ Redirect302 redirectUrl

getOauthRedirectR :: Text -> Text -> AppM Text
getOauthRedirectR code state = do
  ~(SettingAllowRegistration allowRegistration) <- runDB $ getSetting @'AllowRegistration

  -- FIXME(jadel): this doesn't let us get new scopes for existing workspaces
  -- when registration is disabled, but that's probably fine.
  unless allowRegistration $ throwIO RegistrationDisabled

  decoded <- fromEither . mapLeft BadBase64 $ B64.decode . cs $ state
  now <- liftIO getCurrentTime
  mNonceE <- runDB $ do
    deleteExpiredNonces now
    getBy $ UniqueNonceValue decoded

  -- delete it so people can't run it twice (which is invalid on the slack side)
  Entity nonceId _ <- mNonceE `orThrow` BadNonce
  runDB $ delete nonceId

  config <- (.config) <$> getApp
  let clientId = config.slackClientId
      clientSecret = config.slackClientSecret

  oauthResp <- runSlack (error "no token used here") $ \slackConfig ->
    oauth2Access slackConfig $ OAuthRequest {code, clientId, clientSecret}

  teamInfoResp <- runSlack oauthResp.accessToken $ \slackConfig ->
    teamInfo slackConfig $ TeamInfoRequest {domain = Nothing, team = Just oauthResp.team.id}

  _ <- runDB $
    upsertBy
      (UniqueWorkspaceSlackId oauthResp.team.id)
      ( Workspace
          { slackSubdomain = teamInfoResp.team.domain
          , slackTeamId = oauthResp.team.id
          , slackOauthToken = oauthResp.accessToken
          }
      )
      [WorkspaceSlackSubdomain =. teamInfoResp.team.domain]

  pure "Authorized!"
