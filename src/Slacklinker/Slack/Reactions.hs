{-# LANGUAGE TypeOperators #-}

-- | @reactions.remove@, which slack-web doesn't bind yet.
module Slacklinker.Slack.Reactions (RemoveReq (..), RemoveResp (..), reactionsRemove) where

import Data.Aeson qualified as A
import Servant.API (AuthProtect, FormUrlEncoded, JSON, Post, ReqBody, (:>))
import Servant.Client (client)
import Slacklinker.Import
import Web.FormUrlEncoded (ToForm (..))
import Web.Slack.Internal (ResponseJSON, SlackConfig (..), mkSlackAuthenticateReq, run)
import Web.Slack.Pager (Response)
import Web.Slack.Types (ConversationId (..), Emoji (..))

-- | <https://api.slack.com/methods/reactions.remove>
data RemoveReq = RemoveReq
  { removeReqChannel :: ConversationId
  , removeReqName :: Emoji
  , removeReqTimestamp :: Text
  }
  deriving stock (Show)

instance ToForm RemoveReq where
  toForm RemoveReq {..} =
    toForm
      [ ("channel" :: Text, removeReqChannel.unConversationId)
      , ("name", removeReqName.unEmoji)
      , ("timestamp", removeReqTimestamp)
      ]

data RemoveResp = RemoveResp
  deriving stock (Show)

instance FromJSON RemoveResp where
  parseJSON = A.withObject "Reactions.RemoveResp" \_ -> pure RemoveResp

type Api =
  "reactions.remove"
    :> AuthProtect "token"
    :> ReqBody '[FormUrlEncoded] RemoveReq
    :> Post '[JSON] (ResponseJSON RemoveResp)

reactionsRemove :: SlackConfig -> RemoveReq -> IO (Response RemoveResp)
reactionsRemove slackConfig req =
  run (client (Proxy @Api) (mkSlackAuthenticateReq slackConfig) req) slackConfig.slackConfigManager
