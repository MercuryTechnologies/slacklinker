{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Client for Slack @conversations.join@
module Slacklinker.Slack.ConversationsJoin where

import Servant.API
import Servant.Client (ClientM, client)
import Servant.Client.Core (AuthenticatedRequest)
import Slacklinker.Import
import Web.Slack
import Web.Slack.Common (ConversationId)
import Web.Slack.Internal (ResponseJSON, mkSlackAuthenticateReq, run)
import Web.FormUrlEncoded (genericToForm, ToForm (..))
import Web.Slack.Conversation (Conversation)

-- | <https://api.slack.com/methods/conversations.join>
data ConversationJoinRequest = ConversationJoinRequest
  { channel :: ConversationId
  } deriving stock (Eq, Show, Generic)

instance ToForm ConversationJoinRequest where
  toForm = genericToForm snakeCaseFormOptions


{- |
@
{
    "ok": true,
    "channel": {
        "id": "C061EG9SL",
        "name": "general",
        "is_channel": true,
        "is_group": false,
        "is_im": false,
        "created": 1449252889,
        "creator": "U061F7AUR",
        "is_archived": false,
        "is_general": true,
        "unlinked": 0,
        "name_normalized": "general",
        "is_shared": false,
        "is_ext_shared": false,
        "is_org_shared": false,
        "pending_shared": [],
        "is_pending_ext_shared": false,
        "is_member": true,
        "is_private": false,
        "is_mpim": false,
        "topic": {
            "value": "Which widget do you worry about?",
            "creator": "",
            "last_set": 0
        },
        "purpose": {
            "value": "For widget discussion",
            "creator": "",
            "last_set": 0
        },
        "previous_names": []
    },
    "warning": "already_in_channel",
    "response_metadata": {
        "warnings": [
            "already_in_channel"
        ]
    }
}
@
-}
data ConversationsJoinResponse = ConversationsJoinResponse
  { channel :: Conversation
  }
  deriving stock (Show)

$(deriveFromJSON snakeCaseOptions ''ConversationsJoinResponse)

type Api =
  "conversations.join"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] ConversationJoinRequest
    :> Post '[JSON] (ResponseJSON ConversationsJoinResponse)

conversationsJoin ::
  SlackConfig ->
  ConversationJoinRequest ->
  IO (Response ConversationsJoinResponse)
conversationsJoin slackConfig req = do
  let authR = mkSlackAuthenticateReq slackConfig
  run (conversationsJoin_ authR req) . slackConfigManager $ slackConfig

conversationsJoin_ :: AuthenticatedRequest (AuthProtect "token") -> ConversationJoinRequest -> ClientM (ResponseJSON ConversationsJoinResponse)
conversationsJoin_ = client (Proxy @Api)
