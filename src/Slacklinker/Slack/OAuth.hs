{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Client for the Slack OAuth API
module Slacklinker.Slack.OAuth where

import Servant.API
import Servant.Client (ClientM, client)
import Slacklinker.Import
import Web.FormUrlEncoded (ToForm, genericToForm)
import Web.Internal.FormUrlEncoded (ToForm (..))
import Web.Slack.Internal
import Web.Slack.Pager (Response)
import Slacklinker.Types (SlackClientSecret(..), SlackToken)
import Web.Slack.Common (TeamId(..))

data OAuthRequest = OAuthRequest
  { code :: Text
  , clientId :: Text
  , clientSecret :: SlackClientSecret
  }
  deriving stock (Eq, Show, Generic)

instance ToForm OAuthRequest where
  toForm = genericToForm snakeCaseFormOptions

-- | Just a {name, id} pair.
data OAuthEntity = OAuthTeam
  { name :: Text
  , id :: TeamId
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''OAuthEntity)

{- |
See <https://api.slack.com/authentication/oauth-v2>

@
{
    "ok": true,
    "access_token": "xoxb-17653672481-19874698323-pdFZKVeTuE8sk7oOcBrzbqgy",
    "token_type": "bot",
    "scope": "commands,incoming-webhook",
    "bot_user_id": "U0KRQLJ9H",
    "app_id": "A0KRD7HC3",
    "team": {
        "name": "Slack Softball Team",
        "id": "T9TK3CUKW"
    },
    "enterprise": {
        "name": "slack-sports",
        "id": "E12345678"
    },
    "authed_user": {
        "id": "U1234",
        "scope": "chat:write",
        "access_token": "xoxp-1234",
        "token_type": "user"
    }
}
@
-}
data OAuthResponse = OAuthResponse
  { accessToken :: SlackToken
  , tokenType :: Text
  , scope :: Text
  , team :: OAuthEntity
  , enterprise :: Maybe OAuthEntity
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''OAuthResponse)

type OAuthApi =
  "oauth.v2.access" :> ReqBody '[FormUrlEncoded] OAuthRequest :> Post '[JSON] (ResponseJSON OAuthResponse)

oauth2Access ::
  SlackConfig ->
  OAuthRequest ->
  IO (Response OAuthResponse)
oauth2Access slackConfig req = do
  run (oauth2Access_ req) . slackConfigManager $ slackConfig

oauth2Access_ :: OAuthRequest -> ClientM (ResponseJSON OAuthResponse)
oauth2Access_ = client (Proxy @OAuthApi)
