module Slacklinker.Sender.AppHome (doUpdateUser'sAppHome) where

import Data.Vector qualified as V
import Slacklinker.App (HasApp (..))
import Slacklinker.Linear.AppHome (LinearLinkState (..), getLinearLinkState, renderLinearLinkState)
import Slacklinker.Prelude
import Slacklinker.Sender.Internal (runSlack)
import Slacklinker.Sender.Types
import Web.Slack.Experimental.Blocks qualified as BB
import Web.Slack.Experimental.Blocks.Builder qualified as BB
import Web.Slack.Experimental.Views qualified as Views
import Web.Slack.Types (UserId)

renderAppHome :: LinearLinkState -> [BB.SlackBlock]
renderAppHome linkState = runIdentity . BB.runBlockBuilder $ do
  BB.headerBlock "Slacklinker"
  renderLinearLinkState linkState

doUpdateUser'sAppHome :: (HasApp m, MonadIO m) => WorkspaceMeta -> UserId -> m ()
doUpdateUser'sAppHome workspace user = do
  linearState <- getLinearLinkState workspace.workspaceId

  void $ runSlack workspace.token \cfg -> do
    Views.viewsPublish
      cfg
      Views.PublishReq
        { userId = user
        , view =
            Views.SlackView
              { blocks = V.fromList $ renderAppHome linearState
              , privateMetadata = Nothing
              , callbackId = Nothing
              , externalId = Nothing
              , inner = Views.HomeTabView {type_ = Views.Expected}
              }
        }
