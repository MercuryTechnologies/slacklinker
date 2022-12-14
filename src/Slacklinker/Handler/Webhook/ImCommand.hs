module Slacklinker.Handler.Webhook.ImCommand where

import Data.Text qualified as T
import Slacklinker.Import
import Slacklinker.Sender
import Web.Slack.Conversation (ConversationId)
import Web.Slack.Files.Types (FileObject)

data ImCommand
  = Help
  | JoinAll
  | UpdateJoined
  | UploadUserData
  deriving stock (Show)

parseImCommand :: Text -> ImCommand
parseImCommand rawText = case T.strip rawText of
  "help" -> Help
  "join_all" -> JoinAll
  "update_joined" -> UpdateJoined
  "upload_user_data" -> UploadUserData
  _ -> Help

helpMessage :: Text
helpMessage =
  unlines
    [ "Welcome to Slacklinker. Here are the commands you can use:"
    , "* `join_all` - Join all public channels"
    , "* `update_joined` - Update the list of joined channels; allows for better links that include channel names"
    , "* `upload_user_data` - Upload a JSON blob of user data; allows setting user emoji"
    ]

handleImCommand ::
  MonadIO m =>
  WorkspaceMeta ->
  ConversationId ->
  Text ->
  Maybe [FileObject] ->
  m ()
handleImCommand workspaceMeta conversationId message mfiles = do
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
    UploadUserData ->
      for_ (headMay =<< mfiles) $ \f ->
        senderEnqueue $ ReqUploadUserData workspaceMeta conversationId f

  pure ()
