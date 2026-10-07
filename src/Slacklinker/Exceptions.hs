{-# OPTIONS_GHC -Werror=incomplete-patterns #-}

{- | I've contemplated various error handling mechanisms but ultimately what I
 want is @ToServantErr@ instances to get caught and everything else
 500'd.
-}
module Slacklinker.Exceptions where

import Data.Data (cast)
import Data.Text.IO (hPutStrLn)
import Data.Typeable (TypeRep, tyConModule, tyConName, typeOf, typeRepTyCon)
import Network.HTTP.Types
import Network.Wai (Middleware, responseLBS)
import OpenTelemetry.Context qualified as OTel
import OpenTelemetry.Context.ThreadLocal qualified as OTel
import OpenTelemetry.Trace.Core qualified as OTel
import Slacklinker.Prelude
import Slacklinker.SemConv qualified as SemConv
import UnliftIO.Exception qualified as E
import Web.Slack.Common (TeamId (..))
import Web.Slack.Experimental.RequestVerification

-- | Response for some type of exception.
class (Typeable e, Show e) => ExceptionResponse e where
  -- | HTTP status code to return
  status :: e -> Status

  {- | Operation failures default to the concrete exception type. Expected
  outcomes opt out here, alongside their response semantics.
  -}
  classifyServiceException :: e -> ExceptionClassification
  classifyServiceException e = Failed (typeOf e) Nothing

  -- | A human-readable message to include. Default implementation uses 'Show'.
  message :: e -> Text
  message = fromString . show

  {- | Additional headers to include in the response. Content-type headers are
  created by default.
  -}
  headers :: e -> [Header]
  headers _ = []

{- | ServantException does not work since they forgot to include the headers,
 so we define our own. Fun.
-}
data ServiceException = forall e. (Exception e, ExceptionResponse e) => ServiceException e
  deriving anyclass (Exception)

instance Show ServiceException where
  show (ServiceException e) = show e

instance ExceptionResponse ServiceException where
  status (ServiceException e) = status e
  classifyServiceException (ServiceException e) = classifyServiceException e
  message (ServiceException e) = message e
  headers (ServiceException e) = headers e

toServiceException :: (Exception e, ExceptionResponse e) => e -> SomeException
toServiceException = toException . ServiceException

fromServiceException :: (Exception e) => SomeException -> Maybe e
fromServiceException x = fromException x >>= \(ServiceException e) -> cast e

type role DeriveServiceException representational

newtype DeriveServiceException a = DeriveServiceException a

instance (Show e) => Show (DeriveServiceException e) where
  showsPrec p (DeriveServiceException e) = showsPrec p e

instance (ExceptionResponse e) => ExceptionResponse (DeriveServiceException e) where
  status (DeriveServiceException e) = status e
  classifyServiceException (DeriveServiceException e) = classifyServiceException e
  message (DeriveServiceException e) = message e
  headers (DeriveServiceException e) = headers e

instance (ExceptionResponse a) => Exception (DeriveServiceException a) where
  toException = toServiceException
  fromException = fromServiceException

data Redirect302 = Redirect302 {location :: ByteString}
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException Redirect302

instance ExceptionResponse Redirect302 where
  classifyServiceException _ = ExpectedOutcome
  status _ = status302
  message _ = "Redirect"
  headers Redirect302 {location} = [(hLocation, location)]

data AesonDecodeError = AesonDecodeError String
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException AesonDecodeError

instance ExceptionResponse AesonDecodeError where
  status _ = status400

data BadBase64 = BadBase64 String
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException BadBase64

instance ExceptionResponse BadBase64 where
  status _ = status400

data BadNonce = BadNonce
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException BadNonce

instance ExceptionResponse BadNonce where
  status _ = status400

data UnknownWorkspace = UnknownWorkspace TeamId
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException UnknownWorkspace

instance ExceptionResponse UnknownWorkspace where
  status _ = status400

data RegistrationDisabled = RegistrationDisabled
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException RegistrationDisabled

instance ExceptionResponse RegistrationDisabled where
  classifyServiceException _ = ExpectedOutcome
  status _ = status400

-- | Linear integration is disabled
data LinearDisabled = LinearDisabled
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException LinearDisabled

instance ExceptionResponse LinearDisabled where
  classifyServiceException _ = ExpectedOutcome
  status _ = status400

-- | Linear integration is not authenticated
data LinearNotAuthenticated = LinearNotAuthenticated
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException LinearNotAuthenticated

instance ExceptionResponse LinearNotAuthenticated where
  classifyServiceException _ = ExpectedOutcome
  status _ = status400

-- | Linear integration experienced a race condition
data LinearRaced = LinearRaced
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException LinearRaced

instance ExceptionResponse LinearRaced where
  status _ = status400

newtype VerificationException = VerificationException SlackVerificationFailed
  deriving newtype (Show)
  deriving (Exception) via DeriveServiceException VerificationException

instance ExceptionResponse VerificationException where
  classifyServiceException exception@(VerificationException reason) =
    Failed (typeOf exception) $ Just $ SlackVerificationReason reason
  status (VerificationException e) = case e of
    VerificationSignatureMismatch -> status401
    _ -> status400

newtype LinearOAuth2Error = LinearOAuth2Error Text
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException LinearOAuth2Error

instance ExceptionResponse LinearOAuth2Error where
  status _ = status500

newtype SlacklinkerBug = SlacklinkerBug Text
  deriving newtype (Show)
  deriving (Exception) via DeriveServiceException SlacklinkerBug

instance ExceptionResponse SlacklinkerBug where
  status (SlacklinkerBug _) = status500

{- | Keep the runtime type until the telemetry boundary; payloads never become
categories. New exception types need no registration in a separate catalog.
-}
data ExceptionClassification = ExpectedOutcome | Failed TypeRep (Maybe FailureReason)
  deriving stock (Eq, Show)

{- | Retain the domain value until the telemetry boundary. Only its constructor
is emitted as a reason, never the request-specific fields it carries.
-}
data FailureReason = SlackVerificationReason SlackVerificationFailed
  deriving stock (Eq, Show)

{- | Group by the declared type, deliberately excluding type arguments and
package versions. Module qualification distinguishes identically named types.
-}
exceptionTypeName :: TypeRep -> Text
exceptionTypeName representation =
  let constructor = typeRepTyCon representation
   in pack $ tyConModule constructor <> "." <> tyConName constructor

{- | Exhaustiveness is enforced for this module so dependency additions cannot
silently lose a failure reason. No Show instance or payload parsing is involved.
-}
failureReasonName :: FailureReason -> Text
failureReasonName (SlackVerificationReason reason) = case reason of
  VerificationMissingTimestamp -> "VerificationMissingTimestamp"
  VerificationMalformedTimestamp _ -> "VerificationMalformedTimestamp"
  VerificationTimestampOutOfRange _ -> "VerificationTimestampOutOfRange"
  VerificationMissingSignature -> "VerificationMissingSignature"
  VerificationUnknownSignatureVersion _ -> "VerificationUnknownSignatureVersion"
  VerificationMalformedSignature _ -> "VerificationMalformedSignature"
  VerificationUndecodableSignature _ -> "VerificationUndecodableSignature"
  VerificationSignatureMismatch -> "VerificationSignatureMismatch"
  VerificationCannotParse _ -> "VerificationCannotParse"

{- | Service and deriving-via wrappers delegate through ExceptionResponse so
neither their type names nor their implementation details escape into telemetry.
-}
classifyException :: SomeException -> ExceptionClassification
classifyException (SomeException inner)
  | Just service <- cast inner = classifyServiceException (service :: ServiceException)
  | otherwise = Failed (typeOf inner) Nothing

data ErrorSite = HttpRequest | SenderJob
  deriving stock (Eq, Show)

errorSiteText :: ErrorSite -> Text
errorSiteText = \case
  HttpRequest -> "http.request"
  SenderJob -> "sender.job"

{- | Only add classification; the boundary that handles the exception owns its
event and status. In particular, SDK span wrappers already record both.
-}
annotateError :: (MonadIO m) => OTel.Span -> Maybe ErrorSite -> TypeRep -> Maybe FailureReason -> m ()
annotateError span site kind reason = do
  OTel.addAttribute span SemConv.slacklinkerErrorKind $ exceptionTypeName kind
  for_ reason $ OTel.addAttribute span SemConv.slacklinkerErrorReason . failureReasonName
  for_ site $ OTel.addAttribute span SemConv.slacklinkerErrorSite . errorSiteText

errorMiddleware :: Middleware
errorMiddleware baseApp req respond = do
  owner <- OTel.lookupSpan <$> OTel.getContext
  -- WAI tracing ends the span in respond, so record before converting to HTTP.
  let recordFailure e@(SomeException inner) = case classifyException e of
        ExpectedOutcome -> pure ()
        Failed kind reason -> for_ owner $ \span -> do
          annotateError span (Just HttpRequest) kind reason
          OTel.setStatus span $ OTel.Error $ cs $ displayException e
          -- The exception is consumed here and never reaches the SDK wrapper.
          OTel.recordException span mempty Nothing inner
      handlers =
        [ E.Handler
            ( \(e :: ServiceException) -> do
                recordFailure $ toException e
                respond $ responseLBS (status e) (headers e) (cs $ message e)
            )
        , E.Handler
            ( \(e :: SomeException) -> do
                recordFailure e
                -- FIXME(jadel): use monad-logger; needs an instance/monad something here
                hPutStrLn stderr $ "[BUG] Handler exception: " <> tshow e

                respond $ responseLBS status500 [] "Internal server error"
            )
        ]
  baseApp req respond `catches` handlers
