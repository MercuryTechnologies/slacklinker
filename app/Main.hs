module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Function ((&))
import Data.Pool (destroyAllResources)
import Database.Persist.Postgresql (createPostgresqlPool, runMigration)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import OpenTelemetry.Trace qualified as OTel
import Slacklinker.App (App (..), AppConfig (..), RuntimeInfo (..), makeApp, runAppM, runDB)
import Slacklinker.Application
import Slacklinker.Exceptions
import Slacklinker.Import
import Slacklinker.Migrate (migrateAll)
import Slacklinker.Sender (SenderRequest (..), senderEnqueue, senderHandler, senderThread)

appStartup :: App -> IO RuntimeInfo
appStartup app = do
  tracerProvider <- OTel.initializeGlobalTracerProvider
  appPool <- runStderrLoggingT $ createPostgresqlPool app.config.postgresConnectionString 5
  -- if exceptions escape it's a bug
  senderAction <- asyncWithUnmask \unmask -> do
    let app' = app {runtimeInfo = RuntimeInfo {senderAction = error "attempt to access sender action within itself", ..}}
    catch (unmask $ runAppM app' senderThread) (senderHandler "main")
  pure RuntimeInfo {..}

appShutdown :: RuntimeInfo -> IO ()
appShutdown RuntimeInfo {..} = do
  OTel.shutdownTracerProvider tracerProvider
  destroyAllResources appPool
  senderEnqueue $ RequestTerminate
  didFinish <- timeout 1000 $ wait senderAction

  case didFinish of
    Nothing -> uninterruptibleCancel senderAction
    Just () -> pure ()

main :: IO ()
main = do
  app_ <- makeApp
  bracket
    (appStartup app_)
    appShutdown
    $ \rti -> do
      let app = app_ {runtimeInfo = rti}
      let waiApp_ = application app
      otelMiddleware <- newOpenTelemetryWaiMiddleware
      let waiApp = errorMiddleware . otelMiddleware $ waiApp_

      runAppM app $ inSpan "migrate" defaultSpanArguments $ runDB $ runMigration migrateAll
      let settings = defaultSettings & setHost "127.0.0.1" & setPort 4040
      putStrLn "Startup! Listening on 127.0.0.1:4040"
      runSettings settings $ waiApp
