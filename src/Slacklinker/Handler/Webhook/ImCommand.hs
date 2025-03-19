module Slacklinker.Handler.Webhook.ImCommand where

import Data.Text qualified as T
import OpenTelemetry.Trace (addAttribute)
import Slacklinker.Import
import Slacklinker.Sender
import Web.Slack.Conversation (ConversationId)
import Web.Slack.Files.Types (FileObject)

data ImCommand
  = Help
  | JoinAll
  | UpdateJoined
  | UploadUserData
  | UpdateLinearTeams
  deriving stock (Show)

parseImCommand :: Text -> ImCommand
parseImCommand rawText = case T.strip rawText of
  "help" -> Help
  "join_all" -> JoinAll
  "update_joined" -> UpdateJoined
  "upload_user_data" -> UploadUserData
  "update_linear_teams" -> UpdateLinearTeams
  _ -> Help

helpMessage :: Text
helpMessage =
  unlines
    [ "Welcome to Slacklinker. Here are the commands you can use:"
    , "* `join_all` - Join all public channels"
    , "* `update_joined` - Update the list of joined channels; allows for better links that include channel names"
    , "* `upload_user_data` - Upload a JSON blob of user data; allows setting user emoji"
    , "* `update_linear_teams` - Update the set of Linear teams for this Slack workspace"
    ]

handleImCommand ::
  (MonadUnliftIO m) =>
  WorkspaceMeta ->
  ConversationId ->
  Text ->
  Maybe [FileObject] ->
  m ()
handleImCommand workspaceMeta conversationId message mfiles = do
  let cmd = parseImCommand message
  inSpan' "handleImCommand" defaultSpanArguments \span -> do
    addAttribute span "slacklinker.command" $ tshow cmd
    case cmd of
      Help -> send helpMessage
      JoinAll -> senderEnqueue $ ReqJoinAll workspaceMeta conversationId
      UpdateJoined ->
        senderEnqueue
          $ ReqUpdateJoined workspaceMeta conversationId
      UploadUserData -> do
        let file = headMay =<< mfiles
        case file of
          Nothing -> send "No files attached to this message. Send a Slack snippet of JSON to update user data."
          Just f -> senderEnqueue $ ReqUploadUserData workspaceMeta conversationId f
      UpdateLinearTeams -> do
        senderEnqueue $ ReqUpdateLinearTeams workspaceMeta conversationId
  where
    send messageContent =
      senderEnqueue
        . SendMessage
        $ SendMessageReq
          { replyToTs = Nothing
          , messageContent
          , channel = conversationId
          , workspaceMeta
          }
