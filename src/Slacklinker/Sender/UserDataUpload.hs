module Slacklinker.Sender.UserDataUpload (doUploadUserData) where

import Control.Monad.Catch (MonadThrow (..))
import Data.Aeson (eitherDecode)
import Database.Persist
import Network.URI (parseURI)
import OpenTelemetry.Instrumentation.HttpClient (
  Request (..),
  httpClientInstrumentationConfig,
  httpLbs,
  requestFromURI,
  responseBody,
  setRequestCheckStatus,
 )
import Slacklinker.App (App (..), HasApp (..), appSlackConfig, runDB)
import Slacklinker.Import
import Slacklinker.Models
import Slacklinker.Sender.Internal
import Slacklinker.Sender.Types
import Slacklinker.Sender.UserDataUpload.Types
import Slacklinker.Settings (SlacklinkerSetting (..), SlacklinkerSettingTag (..), getSetting)
import Slacklinker.Types (SlackToken (..))
import Web.Slack (usersListAll)
import Web.Slack.Conversation
import Web.Slack.Files.Types (FileMode (..), FileObject (..), FileObjectVisible (..))
import Web.Slack.Pager (loadingPage)
import Web.Slack.User
import Web.Slack.User qualified as User

updateSlackUsersList :: (HasApp m, MonadIO m) => WorkspaceMeta -> m ()
updateSlackUsersList wsInfo = do
  slackConfig <- appSlackConfig wsInfo.token
  users_ <- liftIO $ usersListAll slackConfig (def {User.listReqLimit = Just 100})
  users <- liftIO $ loadingPage users_ convertPage

  runDB $ for_ users \user ->
    let email = user.userProfile >>= (.profileEmail)
     in upsertBy
          (UniqueKnownUser wsInfo.workspaceId user.userId)
          KnownUser
            { workspaceId = wsInfo.workspaceId
            , slackUserId = user.userId
            , email
            , emoji = Nothing
            , githubUsername = Nothing
            }
          [KnownUserEmail =. email]

  pure ()
  where
    convertPage resp =
      fromList @(Vector _) <$> fromEither resp

updateUserData :: MonadIO m => WorkspaceId -> UserDataUpload -> SqlPersistT m ()
updateUserData wsId UserDataUpload {items} = do
  for_ items \item -> do
    updateWhere
      [KnownUserWorkspaceId ==. wsId, KnownUserEmail ==. Just item.email]
      [KnownUserEmoji =. item.callsign, KnownUserGithubUsername =. item.gitHubUsername]

doUploadUserData :: (MonadThrow m, HasApp m, MonadUnliftIO m) => WorkspaceMeta -> ConversationId -> FileObject -> m ()
doUploadUserData wsInfo cid file = do
  ~(SettingAllowUploadUserData permitted) <- runDB $ getSetting @'AllowUploadUserData
  case (permitted, file) of
    (False, _) ->
      logMessage $
        unlines
          [ "Uploading user data is disabled on this Slacklinker instance."
          , "Use `one-off-task set-setting --settingName AllowUploadUserData --value true` on the server to enable it."
          ]
    (True, VisibleFileObject file') | file'.mode == Snippet -> do
      logMessage "Updating Slack user data"
      updateSlackUsersList wsInfo
      logMessage "Reading your data"
      req <- setRequestCheckStatus <$> parseRequest' file'.urlPrivate
      manager <- getsApp (.manager)
      resp <- httpLbs httpClientInstrumentationConfig (addHeaders req) manager

      let decoded = eitherDecode @UserDataUpload $ responseBody resp
      case decoded of
        Left err -> logMessage $ "Error in provided JSON: " <> pack err
        Right content -> do
          runDB $ updateUserData wsInfo.workspaceId content
          logMessage "Done!"

      pure ()
    _ -> logMessage "Invalid file upload, please use a snippet!"
  where
    logMessage messageContent = doSendMessage SendMessageReq {replyToTs = Nothing, channel = cid, messageContent, workspaceMeta = wsInfo}

    -- the following absurdity is due to parseRequest taking a method in the
    -- URI, which is not acceptable to me if it's not statically generated
    parseRequest' :: MonadThrow m => Text -> m Request
    parseRequest' uri =
      requestFromURI
        =<< maybe
          (throwM . stringException $ "bad URI")
          pure
          (parseURI (unpack uri))

    addHeaders req =
      ( req
          { requestHeaders =
              [ ("User-Agent", "slacklinker")
              , ("Authorization", "Bearer " <> encodeUtf8 (unSlackToken wsInfo.token))
              ]
          }
      )
