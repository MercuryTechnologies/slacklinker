{-# LANGUAGE TemplateHaskell #-}

module Slacklinker.Vercel.Types where

import Data.Aeson
import Data.Aeson qualified as A
import Slacklinker.Import

data DeploymentMeta = DeploymentMeta
  { githubPrId :: Text
  , githubOrg :: Text
  , githubRepo :: Text
  , githubCommitSha :: Text
  , githubCommitRef :: Text
  , githubCommitAuthorLogin :: Text
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''DeploymentMeta)

data DeploymentInfo = DeploymentInfo
  { id :: Text
  , meta :: DeploymentMeta
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''DeploymentInfo)

data DeploymentStatusPayload = DeploymentStatusPayload
  { deployment :: DeploymentInfo
  , name :: Text
  , url :: Text
  }
  deriving stock (Show)

$(deriveJSON defaultOptions ''DeploymentStatusPayload)

data Payload
  = DeploymentCreated DeploymentStatusPayload
  | DeploymentError DeploymentStatusPayload
  | DeploymentCanceled DeploymentStatusPayload
  | DeploymentReady DeploymentStatusPayload
  | DeploymentSucceeded DeploymentStatusPayload
  | Other A.Value
  deriving stock (Show)

{- |
 Envelope for a Vercel webhook payload.

 See <https://vercel.com/docs/integrations/webhooks-overview/webhooks-api>

 > {
 >     "id": "-aaaaaaaaaaaaaaa",
 >     "type": "deployment.canceled",
 >     "createdAt": 1669757638081,
 >     "payload": {
 >         "deployment": {
 >             "id": "dpl_AAAAAAAAAAAAAAAAAAAAAAAAAAAA",
 >             "meta": {
 >                 "githubCommitAuthorName": "Joe Smith",
 >                 "githubCommitMessage": "Add kittens",
 >                 "githubCommitOrg": "MercuryTechnologies",
 >                 "githubCommitRef": "some-ref",
 >                 "githubCommitRepo": "slacklinker",
 >                 "githubCommitSha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
 >                 "githubDeployment": "1",
 >                 "githubOrg": "MercuryTechnologies",
 >                 "githubRepo": "slacklinker",
 >                 "githubRepoOwnerType": "Organization",
 >                 "githubCommitRepoId": "123467891",
 >                 "githubRepoId": "123467891",
 >                 "githubCommitAuthorLogin": "joesmith",
 >                 "githubPrId": "1234"
 >             },
 >             "name": "meows-sandbox",
 >             "url": "meows-sandbox.vercel.app",
 >             "inspectorUrl": "https://vercel.com/mercury-technologies/redacted"
 >         },
 >         "links": {
 >             "deployment": "https://vercel.com/mercury-technologies/redacted",
 >             "project": "https://vercel.com/mercury-technologies/redacted"
 >         },
 >         "name": "meows-sandbox",
 >         "plan": "enterprise",
 >         "project": {
 >             "id": "prj_aaaaaaaaaaaaaaaaaaaaaaaaaaaa"
 >         },
 >         "regions": [
 >             "iad1"
 >         ],
 >         "target": null,
 >         "type": "LAMBDAS",
 >         "url": "meows-sandbox.vercel.app",
 >         "user": {
 >             "id": "aaaaaaaaaaaaaaaaaaaaaaaa"
 >         },
 >         "team": {
 >             "id": "team_aaaaaaaaaaaaaaaaaaaaaaaa"
 >         }
 >     }
 > }
-}
data VercelWebhookPayload = VercelWebhookPayload
  { payload :: Payload
  , id :: Text
  , createdAt :: Int64
  , region :: Maybe Text
  }
  deriving stock (Show)

instance FromJSON VercelWebhookPayload where
  parseJSON = withObject "VercelWebhookPayload" \o -> do
    id <- o .: "id"
    createdAt <- o .: "createdAt"
    region <- o .:? "region"
    ty :: Text <- o .: "type"
    payload <- case ty of
      "deployment.created" -> DeploymentCreated <$> o .: "payload"
      "deployment.succeeded" -> DeploymentSucceeded <$> o .: "payload"
      "deployment.ready" -> DeploymentReady <$> o .: "payload"
      "deployment.canceled" -> DeploymentCanceled <$> o .: "payload"
      "deployment.error" -> DeploymentError <$> o .: "payload"
      _ -> Other <$> o .: "payload"

    pure VercelWebhookPayload {..}
