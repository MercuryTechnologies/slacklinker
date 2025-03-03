-- | Linear section of App Home
module Slacklinker.Linear.AppHome where

import Data.StringVariants (mkNonEmptyText)
import Data.StringVariants.NonEmptyText (NonEmptyText, literalNonEmptyText)
import Database.Persist qualified as P
import Slacklinker.App (App (..), AppConfig (..), HasApp (..), runDB)
import Slacklinker.Exceptions (SlacklinkerBug (..))
import Slacklinker.Import
import Slacklinker.Linear.OAuth qualified as Linear
import Slacklinker.Models (LinearOrganization (..), Unique (..), WorkspaceId)
import URI.ByteString (serializeURIRef')
import Web.Slack.Experimental.Blocks.Builder qualified as BB
import Web.Slack.Experimental.Blocks.Types qualified as BB

slackActionDoNothing :: BB.SlackActionId
slackActionDoNothing = BB.SlackActionId $ literalNonEmptyText @"doNothing"

data LinearLinkState
  = -- | Not linked, but here is a URL to link it
    LinearUnlinked (NonEmptyText 3000)
  | -- | Linked already with the given name and url key.
    LinearLinked Text Text
  | -- | Not configured on the instance
    LinearUnavailable

renderLinearLinkState :: (Monad m) => LinearLinkState -> BB.BlockBuilder m ()
renderLinearLinkState = \case
  LinearUnlinked linkUrl -> do
    BB.actionsBlock Nothing . BB.toSlackActionList $ buttonWithUrlAndMessage linkUrl "Link Linear"
  LinearUnavailable -> do
    BB.sectionBlock "Linear unavailable"
  LinearLinked name urlKey -> do
    BB.sectionBlock $ "Linear linked to " <> BB.link (linearOrgUrl urlKey) (Just name)
  where
    linearOrgUrl = ("https://linear.app/" <>)
    buttonWithUrlAndMessage url message =
      BB.button
        slackActionDoNothing
        message
        ( BB.ButtonSettings
            { buttonUrl = BB.setting url
            , buttonValue = BB.emptySetting
            , buttonStyle = BB.setting BB.SlackStylePrimary
            , buttonConfirm = BB.emptySetting
            }
        )

{- | Gets the link state and if possible makes a URL that can link the
workspace
-}
getLinearLinkState :: (MonadIO m, HasApp m) => WorkspaceId -> m LinearLinkState
getLinearLinkState wsId = do
  bitsMay <- getsApp (\a -> (,) <$> a.config.slacklinkerHost <*> a.config.linearCreds)
  case bitsMay of
    Nothing -> pure LinearUnavailable
    Just (host, creds) -> do
      mLinearOrg <- runDB . P.getBy $ UniqueOneLinearOrganizationPerTenant wsId
      case entityVal <$> mLinearOrg of
        Just o -> pure $ LinearLinked o.displayName o.urlKey
        Nothing -> do
          authUri <- Linear.makeAuthorizationURI host wsId creds
          LinearUnlinked
            <$> ( mkNonEmptyText (decodeUtf8 $ serializeURIRef' authUri)
                    `orThrow` SlacklinkerBug "linear authorization uri extremely long"
                )
