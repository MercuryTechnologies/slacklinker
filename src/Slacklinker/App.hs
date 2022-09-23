module Slacklinker.App
  ( AppM (..),
    HasApp (..),
    RuntimeInfo (..),
    App (..),
    AppConfig (..),
    appSlackConfig,
    runAppM,
    runDB,
    getConfiguration,
    makeApp,
  )
where

import Control.Monad.Logger (ToLogStr (..), defaultOutput)
import Data.Pool (Pool)
import Database.Persist.Postgresql (SqlBackend, runSqlPoolWithExtensibleHooks)
import Database.Persist.SqlBackend.SqlPoolHooks
import Network.HTTP.Client (Manager)
import OpenTelemetry.Instrumentation.Persistent qualified as OTel
import OpenTelemetry.Trace (TracerProvider)
import Slacklinker.Import
import Slacklinker.Types (SlackClientSecret(..), SlackSigningSecret(..), SlackToken (..))
import Web.Slack (SlackConfig (..))
import qualified Data.ByteString.Char8 as BS
import System.Environment (getEnv)
import Network.HTTP.Client.TLS (newTlsManager)

data AppConfig = AppConfig
  { slackClientSecret :: SlackClientSecret
  , slackSigningSecret :: SlackSigningSecret
  , slackClientId :: Text
  , postgresConnectionString :: ByteString
  }
  deriving stock (Show)

appSlackConfig :: HasApp m => SlackToken -> m SlackConfig
appSlackConfig (SlackToken slackConfigToken) = do
  slackConfigManager <- getsApp (.manager)
  pure SlackConfig {..}

-- | Stuff that needs to be torn down on shutdown but isn't actually used by
--   anything in ghci.
data RuntimeInfo = RuntimeInfo
  { tracerProvider :: TracerProvider
  , senderAction :: Async ()
  , appPool :: Pool SqlBackend
  }

data App = App
  { config :: AppConfig
  , manager :: Manager
  , runtimeInfo :: RuntimeInfo
  }

class (Monad m, MonadLogger m) => HasApp m where
  getApp :: m App
  getsApp :: (App -> a) -> m a
  getsApp f = f <$> getApp

newtype AppM a = AppM {unAppM :: ReaderT App IO a}
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadUnliftIO)

instance MonadLogger AppM where
  monadLoggerLog loc source level msg = do
    liftIO $ defaultOutput stderr loc source level $ toLogStr msg

instance HasApp AppM where
  getApp = do
    AppM ask

runAppM :: App -> AppM a -> IO a
runAppM app act = do
  let readerT = unAppM act
  runReaderT readerT app

runDB :: (HasApp m, MonadIO m) => SqlPersistT IO a -> m a
runDB act = do
  app <- getApp
  let pool = app.runtimeInfo.appPool
  liftIO $ runSqlPoolWithExtensibleHooks act pool Nothing hooks
  where
    hooks = setAlterBackend defaultSqlPoolHooks $ OTel.wrapSqlBackend []

getConfiguration :: IO AppConfig
getConfiguration = do
  slackSigningSecret <- SlackSigningSecret <$> getEnvBS "SLACK_SIGNING_SECRET"
  slackClientSecret <- SlackClientSecret . cs <$> getEnvBS "SLACK_CLIENT_SECRET"
  slackClientId <- cs <$> getEnvBS "SLACK_CLIENT_ID"
  postgresConnectionString <- getEnvBS "POSTGRES_CONNECTION_STRING"
  pure AppConfig {..}
  where
    getEnvBS :: String -> IO ByteString
    getEnvBS a = BS.pack <$> getEnv a

makeApp :: IO App
makeApp = do
  -- FIXME(jadel): instrument
  manager <- newTlsManager
  config <- getConfiguration
  let runtimeInfo = error "runtime info not initialized yet. this is a bug"
  pure App {..}
