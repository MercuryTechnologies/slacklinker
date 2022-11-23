module Main where

import OpenTelemetry.Instrumentation.Hspec (wrapSpec)
import Slacklinker.Prelude
import Slacklinker.Tracing
import Spec qualified
import Test.Hspec
import Test.Hspec.Runner (defaultConfig, hspecWith)

main :: IO ()
main = do
  putStrLn "Begin tests"
  withGlobalTracing $
    inSpan "Run tests" defaultSpanArguments runTests

runTests :: IO ()
runTests = do
  wrapper <- wrapSpec
  hspecWith
    defaultConfig
    $ wrapper (parallel Spec.spec)
  putStrLn "Done"
