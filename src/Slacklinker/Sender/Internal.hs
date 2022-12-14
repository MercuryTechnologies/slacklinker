module Slacklinker.Sender.Internal where

import Slacklinker.App (HasApp (..), appSlackConfig)
import Slacklinker.Import
import Slacklinker.Sender.Types
import Slacklinker.Types (SlackToken (..))
import Web.Slack (SlackConfig, chatPostMessage)
import Web.Slack.Chat (PostMsgReq (..), mkPostMsgReq)
import Web.Slack.Common (SlackClientError)
import Web.Slack.Types (ConversationId (..))

runSlack :: (MonadIO m, HasApp m) => SlackToken -> (SlackConfig -> IO (Either SlackClientError a)) -> m a
runSlack workspaceToken act = do
  slackConfig <- appSlackConfig workspaceToken
  fromEitherIO . liftIO $ act slackConfig

doSendMessage :: (HasApp m, MonadIO m) => SendMessageReq -> m ()
doSendMessage req = do
  let postMsgReq = (mkPostMsgReq req.channel.unConversationId req.messageContent) {postMsgReqThreadTs = req.replyToTs}
  _ <- runSlack req.workspaceMeta.token $ \slackConfig -> chatPostMessage slackConfig postMsgReq
  pure ()
