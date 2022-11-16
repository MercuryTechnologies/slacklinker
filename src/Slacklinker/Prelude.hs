module Slacklinker.Prelude (
  module ClassyPrelude,
  module Control.Monad.Logger.CallStack,
  module Data.Aeson,
  module Data.Aeson.TH,
  module Data.Either.Combinators,
  module Servant,
  module Servant.Server,
  module Data.Proxy,
  module Database.Persist.Sql,
  module Data.Default.Class,
  module Database.Persist,
  cs,
  identity,
) where

import ClassyPrelude hiding (Handler, delete, deleteBy, span, on, link2, id)
import ClassyPrelude qualified as Prelude
import Control.Monad.Logger.CallStack (MonadLogger (..), logDebug, logError, logInfo, logWarn)
import Data.Aeson (FromJSON (..), ToJSON (..), defaultOptions, withObject, withText)
import Data.Aeson.TH (deriveFromJSON, deriveJSON, deriveToJSON)
import Data.Default.Class
import Data.Either.Combinators (mapLeft, mapRight)
import Data.Proxy (Proxy (..))
import Data.String.Conversions (cs)
import Database.Persist (Entity (..))
import Database.Persist.Sql (SqlPersistT)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Servant.Server (Handler)

identity :: t -> t
identity = Prelude.id
