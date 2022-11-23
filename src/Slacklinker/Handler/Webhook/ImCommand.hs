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
parseImCommand rawText = case T.strip rawText of
  "help" -> Help
  "join_all" -> JoinAll
  "update_joined" -> UpdateJoined
  _ -> Help

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
