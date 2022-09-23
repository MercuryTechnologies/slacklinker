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

newtype Greet = Greet {msg :: Text}
  deriving stock (Show)

$(deriveToJSON defaultOptions ''Greet)

type RequiredHeader = Header' '[Required, Strict]

type Api =
  "hello" :> Get '[JSON] Greet
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
server = helloH :<|> webhookH :<|> authorizeH :<|> oauthRedirectH
  where
    authorizeH = getAuthorizeR
    oauthRedirectH = getOauthRedirectR
    helloH = return . Greet $ "blah"
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
