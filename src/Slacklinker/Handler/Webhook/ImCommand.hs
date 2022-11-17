module Slacklinker.Handler.Webhook.ImCommand where

import Data.Text qualified as T
import Slacklinker.Import
import Slacklinker.Sender
import Web.Slack.Conversation (ConversationId)

data ImCommand
  = Help
  | JoinAll
  | UpdateJoined
  deriving stock (Show)

parseImCommand :: Text -> ImCommand
parseImCommand rawText = go $ T.strip rawText
  where
    go "help" = Help
    go "join_all" = JoinAll
    go "update_joined" = UpdateJoined
    go _ = Help

helpMessage :: Text
helpMessage =
  unlines
    [ "Welcome to Slacklinker. Here are the commands you can use:"
    , "* `join_all` - Join all public channels"
    , "* `update_joined` - Update the list of joined channels; allows for better links that include channel names"
    ]

handleImCommand ::
  MonadIO m =>
  WorkspaceMeta ->
  ConversationId ->
  Text ->
  m ()
handleImCommand workspaceMeta conversationId message = do
  let cmd = parseImCommand message
  case cmd of
    Help ->
      senderEnqueue . SendMessage $
        SendMessageReq
          { replyToTs = Nothing
          , channel = conversationId
          , messageContent = helpMessage
          , workspaceMeta
          }
    JoinAll -> senderEnqueue $ ReqJoinAll workspaceMeta conversationId
    UpdateJoined ->
      senderEnqueue $
        ReqUpdateJoined workspaceMeta conversationId

  pure ()
