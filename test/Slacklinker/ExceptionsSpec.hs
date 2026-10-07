module Slacklinker.ExceptionsSpec (spec) where

import Data.Typeable (typeOf)
import Network.HTTP.Types (status418)
import Slacklinker.Exceptions
import TestImport
import Web.Slack.Common (TeamId (..))
import Web.Slack.Experimental.RequestVerification

-- Deliberately absent from production code: classification must work without
-- adding this type to a registry, including through deriving-via wrappers.
newtype NewServiceError = NewServiceError Text
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException NewServiceError

instance ExceptionResponse NewServiceError where
  status _ = status418

type role ParameterizedError representational

newtype ParameterizedError a = ParameterizedError a

serviceForms :: (Exception e, ExceptionResponse e) => e -> [SomeException]
serviceForms exception =
  [ toException exception
  , toException (ServiceException exception)
  , toException (ServiceException (ServiceException exception))
  , toException (DeriveServiceException exception)
  ]

spec :: Spec
spec = describe "exception classification" do
  let failure :: (Exception e, ExceptionResponse e) => e -> Expectation
      failure exception =
        for_ (serviceForms exception) $ \wrapped ->
          classifyException wrapped `shouldBe` Failed (typeOf exception) Nothing

  it "discovers new service exception types through service and deriving-via wrappers" do
    failure $ NewServiceError "request-specific details"
    failure $ LinearOAuth2Error "upstream failed"
    failure $ SlacklinkerBug "invariant failed"

  it "names the declared type with its module, excluding type arguments and payloads" do
    exceptionTypeName (typeOf $ ParameterizedError @Int 42)
      `shouldBe` "Slacklinker.ExceptionsSpec.ParameterizedError"
    exceptionTypeName (typeOf $ ParameterizedError @Text "request-specific data")
      `shouldBe` "Slacklinker.ExceptionsSpec.ParameterizedError"

  it "retains the inner verification reason through every service wrapper" do
    let reason = VerificationMalformedTimestamp "request-specific data"
        exception = VerificationException reason
    for_ (serviceForms exception) $ \wrapped ->
      classifyException wrapped `shouldBe` Failed (typeOf exception) (Just $ SlackVerificationReason reason)

  it "keeps invalid input, unknown workspaces and races as failures" do
    failure $ AesonDecodeError "invalid JSON"
    failure $ BadBase64 "invalid encoding"
    failure BadNonce
    failure $ UnknownWorkspace $ TeamId "T123"
    failure LinearRaced

  it "uses runtime types for external exceptions, independently of their messages" do
    let exception = userError "BadNonce"
    classifyException (toException exception) `shouldBe` Failed (typeOf exception) Nothing
    classifyException (toException exception)
      `shouldBe` classifyException (toException $ userError "different failure")
    classifyException (toException exception)
      `shouldNotBe` classifyException (toException $ NewServiceError "BadNonce")

  it "does not put workspace IDs or exception messages into categories" do
    classifyException (toException $ UnknownWorkspace $ TeamId "T456")
      `shouldBe` classifyException (toException $ UnknownWorkspace $ TeamId "T789")
    classifyException (toException $ NewServiceError "one message")
      `shouldBe` classifyException (toException $ NewServiceError "another message")

  for_
    [ ("redirects", serviceForms $ Redirect302 "/authorize")
    , ("disabled registration", serviceForms RegistrationDisabled)
    , ("disabled Linear integration", serviceForms LinearDisabled)
    , ("missing Linear authentication", serviceForms LinearNotAuthenticated)
    ]
    $ \(label, forms) ->
      it ("recognizes " <> label <> " as expected through every wrapper")
        $ for_ forms
        $ \wrapped -> classifyException wrapped `shouldBe` ExpectedOutcome
