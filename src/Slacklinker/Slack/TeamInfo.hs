{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Client for Slack @team.info@
module Slacklinker.Slack.TeamInfo where

import Servant.API
import Servant.Client (ClientM, client)
import Servant.Client.Core (AuthenticatedRequest)
import Slacklinker.Import
import Web.Slack
import Web.Slack.Common (TeamId)
import Web.Slack.Internal (ResponseJSON, mkSlackAuthenticateReq, run)
import Web.FormUrlEncoded (genericToForm, ToForm (..))

-- | <https://api.slack.com/methods/team.info>
data TeamInfoRequest = TeamInfoRequest
  { domain :: Maybe Text
  -- ^ \"Query by domain instead of team (only when team is null). This only
  -- works for domains in the same enterprise as the querying team token. This
  -- also expects the domain to belong to a team and not the enterprise itself.\"
  , team :: Maybe TeamId
  -- ^ \"Team to get info on, if omitted, will return information about the
  -- current team. Will only return team that the authenticated token is
  -- allowed to see through external shared channels\"
  } deriving stock (Eq, Show, Generic)

instance ToForm TeamInfoRequest where
  toForm = genericToForm snakeCaseFormOptions

data TeamInfo = TeamInfo
  { id :: TeamId
  , name :: Text
  , domain :: Text
  , emailDomain :: Text
  , enterpriseId :: Maybe Text
  , enterpriseName :: Maybe Text
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''TeamInfo)

{- |
@
{
    "ok": true,
    "team": {
        "id": "T12345",
        "name": "My Team",
        "domain": "example",
        "email_domain": "example.com",
        "icon": {
            "image_34": "https://...",
            "image_44": "https://...",
            "image_68": "https://...",
            "image_88": "https://...",
            "image_102": "https://...",
            "image_132": "https://...",
            "image_default": true
        },
        "enterprise_id": "E1234A12AB",
        "enterprise_name": "Umbrella Corporation"
    }
}
@
-}
data TeamInfoResponse = TeamInfoResponse
  { team :: TeamInfo
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''TeamInfoResponse)

type Api =
  "team.info"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] TeamInfoRequest
    :> Post '[JSON] (ResponseJSON TeamInfoResponse)

teamInfo ::
  SlackConfig ->
  TeamInfoRequest ->
  IO (Response TeamInfoResponse)
teamInfo slackConfig req = do
  let authR = mkSlackAuthenticateReq slackConfig
  run (teamInfo_ authR req) . slackConfigManager $ slackConfig

teamInfo_ :: AuthenticatedRequest (AuthProtect "token") -> TeamInfoRequest -> ClientM (ResponseJSON TeamInfoResponse)
teamInfo_ = client (Proxy @Api)
