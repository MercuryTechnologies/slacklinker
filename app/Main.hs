module Main where

import Data.Function ((&))
import Network.Wai.Handler.Warp (defaultSettings, runSettings, setHost, setPort)
import OpenTelemetry.Instrumentation.Wai (newOpenTelemetryWaiMiddleware)
import Slacklinker.App (App (..), RuntimeInfo (..), appShutdownNoSender, appStartupNoSender, makeApp, runAppM)
import Slacklinker.Application
import Slacklinker.Exceptions
import Slacklinker.Import
import Slacklinker.Sender (SenderRequest (..), senderEnqueue, senderHandler, senderThread)

appStartup :: App -> IO RuntimeInfo
appStartup app = do
  rtInfo <- appStartupNoSender app
  -- if exceptions escape it's a bug
  senderAction <- asyncWithUnmask \unmask -> do
    let app' = app {runtimeInfo = rtInfo}
    catch (unmask $ runAppM app' senderThread) (senderHandler "main")
  pure rtInfo {senderAction}

appShutdown :: RuntimeInfo -> IO ()
appShutdown rtInfo@RuntimeInfo {..} = do
  senderEnqueue RequestTerminate
  didFinish <- timeout 1000 $ wait senderAction

  case didFinish of
    Nothing -> uninterruptibleCancel senderAction
    Just () -> pure ()
  -- teardown the db connection etc after the sender is known to be stopped
  appShutdownNoSender rtInfo

main :: IO ()
main = do
  app_ <- makeApp
  withGlobalTracing
    $ bracket
      (appStartup app_)
      appShutdown
    $ \rti -> do
      let app = app_ {runtimeInfo = rti}
      let waiApp_ = application app
      otelMiddleware <- newOpenTelemetryWaiMiddleware
      let waiApp = errorMiddleware . otelMiddleware $ waiApp_

      let settings = defaultSettings & setHost "127.0.0.1" & setPort 4040
      putStrLn "Startup! Listening on 127.0.0.1:4040"
      runSettings settings waiApp
