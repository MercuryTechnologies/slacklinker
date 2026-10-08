{-# LANGUAGE TupleSections #-}

module Slacklinker.TaskSpec (spec) where

import Control.Exception qualified as Exception
import Data.HashMap.Strict qualified as HashMap
import OpenTelemetry.Attributes qualified as Attr
import OpenTelemetry.Context qualified as Context
import OpenTelemetry.Context.ThreadLocal qualified as Context
import OpenTelemetry.Processor.Span (ShutdownResult (..), SpanProcessor (..))
import OpenTelemetry.Trace qualified as OTel
import OpenTelemetry.Trace.Sampler (alwaysOn)
import OpenTelemetry.Util (appendOnlyBoundedCollectionSize)
import Options.Generic (Unwrapped)
import Slacklinker.Settings (SlacklinkerSettingTag (..))
import Slacklinker.Task
import Slacklinker.Tracing qualified as Tracing
import TestImport

data TaskFailure = TaskFailure
  deriving stock (Eq, Show)
  deriving anyclass (Exception)

spec :: Spec
spec = sequential $ describe "maintenance command tracing" do
  for_ ["success", "configuration", "startup", "execution", "teardown"] \failure ->
    it ("includes the application lifetime and shuts down after " <> unpack failure) do
      let phase record name = do
            record name
            when (failure == name) $ throwIO TaskFailure
      (result, spans, events) <- capture (SuggestMigrations "add_index" False) \_ record -> do
        phase record "configuration"
        bracket (phase record "startup") (const $ phase record "teardown") \_ ->
          phase record "execution"
      case failure of
        "success" -> result `shouldSatisfy` either (const False) (const True)
        _ -> either fromException (const Nothing) result `shouldBe` Just TaskFailure
      events `shouldBe` case failure of
        "configuration" -> ["span-start", "configuration", "span-end", "shutdown"]
        "startup" -> ["span-start", "configuration", "startup", "span-end", "shutdown"]
        _ -> ["span-start", "configuration", "startup", "execution", "teardown", "span-end", "shutdown"]
      owner <- onlyOwner spans
      attribute owner "maintenance.command.category" ("suggest-migrations" :: Text)
      attribute owner "maintenance.migration.name" ("add_index" :: Text)
      checkOutcome owner $ if failure == "success" then "success" else "failure"
      absent owner "maintenance.organizations.planned"

  for_ [(UpdateAllLinearTeams, "update-all-linear-teams"), (MigrateLinearTokens, "migrate-linear-tokens")] \(task, category) -> do
    it (unpack category <> " leaves the plan unknown when setup fails") do
      (_, spans, _) <- capture task \_ _ -> throwIO TaskFailure
      owner <- onlyOwner spans
      attribute owner "maintenance.command.category" (category :: Text)
      checkOutcome owner "failure"
      absent owner "maintenance.organizations.planned"
      checkCounts owner 0 0 0

    -- Exhaust every possible failure position in small batches, plus success
    -- and empty plans, without touching a database or an external API.
    for_ [0 .. 5 :: Int] \planned ->
      for_ (Nothing : map Just [1 .. planned]) \failureAt ->
        it (unpack category <> ": plan " <> show planned <> ", failure " <> show failureAt) do
          attempted <- newIORef []
          (result, spans, events) <- capture task \owner _ ->
            runOrganizations owner [1 .. planned] \organization -> do
              modifyIORef' attempted (<> [organization])
              when (failureAt == Just organization) $ throwIO TaskFailure
          owner <- onlyOwner spans
          attribute owner "maintenance.command.category" (category :: Text)
          attribute owner "maintenance.organizations.planned" planned
          events `shouldBe` ["span-start", "span-end", "shutdown"]
          case failureAt of
            Nothing -> do
              result `shouldSatisfy` either (const False) (const True)
              readIORef attempted >>= (`shouldBe` [1 .. planned])
              checkCounts owner planned planned 0
              checkOutcome owner "success"
            Just n -> do
              either fromException (const Nothing) result `shouldBe` Just TaskFailure
              readIORef attempted >>= (`shouldBe` [1 .. n])
              checkCounts owner n (n - 1) 1
              checkOutcome owner "failure"

  it "retains completed work when teardown fails" do
    (_, spans, _) <- capture MigrateLinearTokens \owner _ ->
      bracket_ (pure ()) (throwIO TaskFailure)
        $ runOrganizations owner [1 .. 3 :: Int] (const $ pure ())
    owner <- onlyOwner spans
    checkCounts owner 3 3 0
    checkOutcome owner "failure"

  it "summarizes cancellation and rethrows it after ending the span" do
    (result, spans, events) <- capture MigrateLinearTokens \owner _ ->
      runOrganizations owner [1 .. 3 :: Int] \n ->
        when (n == 2) $ Exception.throwIO Exception.ThreadKilled
    either fromException (const Nothing) result `shouldBe` Just Exception.ThreadKilled
    owner <- onlyOwner spans
    checkCounts owner 2 1 1
    checkOutcome owner "failure"
    events `shouldBe` ["span-start", "span-end", "shutdown"]

  it "enriches the owner even while a dependency span is current" do
    (_, spans, _) <- capture UpdateAllLinearTeams \owner _ ->
      Tracing.inSpan "dependency" Tracing.defaultSpanArguments
        $ runOrganizations owner [()] (const $ pure ())
    owner <- onlyOwner $ filter ((== "maintenance.command") . OTel.spanName) spans
    checkCounts owner 1 1 0
    let children = filter ((== "dependency") . OTel.spanName) spans
    length children `shouldBe` 1
    for_ children \child -> absent child "maintenance.organizations.completed"

  it "records only the setting name, even if an error contains a secret value" do
    let secret = "fake-secret-token-and-connection-string"
    (result, spans, _) <- capture (SetSetting AllowRegistration $ cs secret) \_ _ ->
      Exception.throwIO $ userError secret
    either fromException (const Nothing) result `shouldBe` Just (userError secret)
    owner <- onlyOwner spans
    attribute owner "maintenance.command.category" ("set-setting" :: Text)
    attribute owner "maintenance.setting.name" ("AllowRegistration" :: Text)
    checkOutcome owner "failure"
    -- Allow SDK source/thread metadata, but reject any additional payload field.
    let sdkFields = ["code.lineno", "code.namespace", "code.package", "code.filepath", "code.function", "thread.id"]
    HashMap.filterWithKey (\key _ -> key `notElem` sdkFields) (Attr.getAttributeMap owner.spanAttributes)
      `shouldBe` HashMap.fromList
        [ ("maintenance.command.category", Attr.toAttribute ("set-setting" :: Text))
        , ("maintenance.command.outcome", Attr.toAttribute ("failure" :: Text))
        , ("maintenance.setting.name", Attr.toAttribute ("AllowRegistration" :: Text))
        ]

{- | Run only injected in-memory work through the production span and provider
lifetime helpers. Restore global and thread-local state for the rest of Hspec.
-}
capture :: Task Unwrapped -> (OTel.Span -> (Text -> IO ()) -> IO ()) -> IO (Either SomeException (), [OTel.ImmutableSpan], [Text])
capture task action = bracket Context.getContext (void . Context.attachContext) \_ ->
  bracket OTel.getGlobalTracerProvider OTel.setGlobalTracerProvider \_ -> do
    void $ Context.attachContext Context.empty
    finished <- newIORef []
    events <- newIORef []
    let record event = modifyIORef' events (<> [event])
        processor =
          SpanProcessor
            { spanProcessorOnStart = \_ _ -> record "span-start"
            , spanProcessorOnEnd = \ref -> do
                readIORef ref >>= \s -> modifyIORef' finished (<> [s])
                record "span-end"
            , spanProcessorShutdown = async $ record "shutdown" >> pure ShutdownSuccess
            , spanProcessorForceFlush = pure ()
            }
        acquire = do
          provider <- OTel.createTracerProvider [processor] OTel.emptyTracerProviderOptions {OTel.tracerProviderOptionsSampler = alwaysOn}
          OTel.setGlobalTracerProvider provider
          pure provider
    result <- Exception.try $ Tracing.withTracingProvider acquire $ withTaskSpan task \owner -> action owner record
    (result,,) <$> readIORef finished <*> readIORef events

onlyOwner :: [OTel.ImmutableSpan] -> IO OTel.ImmutableSpan
onlyOwner spans = do
  map OTel.spanName spans `shouldBe` ["maintenance.command"]
  case spans of
    [owner] -> do
      owner.spanEnd `shouldSatisfy` maybe False (>= owner.spanStart)
      Attr.getDropped owner.spanAttributes `shouldBe` 0
      appendOnlyBoundedCollectionSize owner.spanEvents `shouldBe` 0
      pure owner
    _ -> throwIO $ userError "Expected one owning command span"

checkOutcome :: OTel.ImmutableSpan -> Text -> Expectation
checkOutcome owner outcome = do
  attribute owner "maintenance.command.outcome" outcome
  when (outcome == "failure")
    $ owner.spanStatus
    `shouldBe` OTel.Error "Maintenance command failed"

checkCounts :: OTel.ImmutableSpan -> Int -> Int -> Int -> Expectation
checkCounts owner attempted completed failed = do
  attribute owner "maintenance.organizations.attempted" attempted
  attribute owner "maintenance.organizations.completed" completed
  attribute owner "maintenance.organizations.failed" failed

attribute :: (Attr.ToAttribute a) => OTel.ImmutableSpan -> Text -> a -> Expectation
attribute owner name value = Attr.lookupAttribute owner.spanAttributes name `shouldBe` Just (Attr.toAttribute value)

absent :: OTel.ImmutableSpan -> Text -> Expectation
absent owner name = Attr.lookupAttribute owner.spanAttributes name `shouldBe` Nothing
