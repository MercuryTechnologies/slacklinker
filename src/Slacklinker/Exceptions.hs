{- | I've contemplated various error handling mechanisms but ultimately what I
 want is @ToServantErr@ instances to get caught and everything else
 500'd.
-}
module Slacklinker.Exceptions where

import Data.Data (cast)
import Data.Text.IO (hPutStrLn)
import Network.HTTP.Types
import Network.Wai (Middleware, responseLBS)
import Slacklinker.Prelude
import UnliftIO.Exception qualified as E
import Web.Slack.Common (TeamId (..))

-- | Response for some type of exception.
class (Typeable e, Show e) => ExceptionResponse e where
  -- | HTTP status code to return
  status :: e -> Status

  -- | A human-readable message to include. Default implementation uses 'Show'.
  message :: e -> Text
  message = fromString . show

  -- | Additional headers to include in the response. Content-type headers are
  -- created by default.
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
  message (ServiceException e) = message e
  headers (ServiceException e) = headers e

toServiceException :: (Exception e, ExceptionResponse e) => e -> SomeException
toServiceException = toException . ServiceException

fromServiceException :: Exception e => SomeException -> Maybe e
fromServiceException x = fromException x >>= \(ServiceException e) -> cast e

newtype DeriveServiceException a = DeriveServiceException a

instance Show e => Show (DeriveServiceException e) where
  showsPrec p (DeriveServiceException e) = showsPrec p e

instance ExceptionResponse e => ExceptionResponse (DeriveServiceException e) where
  status (DeriveServiceException e) = status e
  message (DeriveServiceException e) = message e
  headers (DeriveServiceException e) = headers e

instance (ExceptionResponse a) => Exception (DeriveServiceException a) where
  toException = toServiceException
  fromException = fromServiceException

data Redirect302 = Redirect302 {location :: ByteString}
  deriving stock (Show)
  deriving (Exception) via DeriveServiceException Redirect302

instance ExceptionResponse Redirect302 where
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
  status _ = status400

errorMiddleware :: Middleware
errorMiddleware baseApp req respond =
  baseApp req respond `catches` handlers
  where
    handlers =
      [ E.Handler (\(e :: ServiceException) -> respond $ responseLBS (status e) (headers e) (cs $ message e))
      , E.Handler
          ( \(e :: SomeException) -> do
              -- FIXME(jadel): use monad-logger; needs an instance/monad something here
              hPutStrLn stderr $ "[BUG] Handler exception: " <> tshow e

              respond $ responseLBS status500 [] "Internal server error"
          )
      ]
