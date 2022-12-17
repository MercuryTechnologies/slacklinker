module Slacklinker.Vercel.TypesSpec where

import TestImport
import Slacklinker.Vercel.Types
import JSONGolden

spec :: Spec
spec = describe "Vercel webhooks" do
  describe "Deployment status" do
    mapM_ (oneGoldenTest @VercelWebhookPayload) ["deployment_canceled"]
