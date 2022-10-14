{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

module Slacklinker.Application where

import Data.Aeson (Value)
import Servant
import Slacklinker.App (App, AppM, runAppM)
import Slacklinker.Handler.Authorize (getAuthorizeR, getOauthRedirectR)
import Slacklinker.Handler.Webhook (postSlackInteractiveWebhookR)
import Slacklinker.Import
import Slacklinker.Types
import Web.Slack.Experimental.RequestVerification

newtype Healthcheck = Healthcheck {alive :: Bool}
  deriving stock (Show)

$(deriveToJSON defaultOptions ''Healthcheck)

type RequiredHeader = Header' '[Required, Strict]

type Api =
  "alive" :> Get '[JSON] Healthcheck
    :<|> "webhook"
      :> RequiredHeader "X-Slack-Signature" SlackSignature
      :> RequiredHeader "X-Slack-Request-Timestamp" SlackRequestTimestamp
      :> ReqBody '[JSONByteString] ByteString
      :> Post '[JSON] Value
    :<|> "authorize" :> Get '[PlainText] Text
    :<|> "oauth_redirect"
      :> QueryParam' '[Required] "code" Text
      :> QueryParam' '[Required] "state" Text
      :> Get '[PlainText] Text

context :: Context '[]
context = EmptyContext

server :: ServerT Api AppM
server = aliveH :<|> webhookH :<|> authorizeH :<|> oauthRedirectH
  where
    authorizeH = getAuthorizeR
    oauthRedirectH = getOauthRedirectR
    aliveH = return . Healthcheck $ True
    webhookH sig ts body = postSlackInteractiveWebhookR sig ts body

api :: Proxy Api
api = Proxy

application :: App -> Application
application app = serveWithContextT api context unliftApp server
  where
    unliftApp :: AppM a -> Handler a
    unliftApp act = do
      -- there is no attempt to deal with exceptions here. we use middleware to
      -- deal with them instead.
      liftIO $ runAppM app act
