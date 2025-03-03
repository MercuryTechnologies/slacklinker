module Slacklinker.Linear.GraphQL (runQuery, resultThrow) where

import Data.GraphQL (GraphQLResult (..), GraphQLSettings (..))
import Data.GraphQL.Error (GraphQLException (..))
import Data.GraphQL.Monad (GraphQLQueryT, MonadGraphQLQuery, defaultGraphQLSettings, runGraphQLQueryT)
import Network.HTTP.Client (Request (..))
import Network.HTTP.Types (hAuthorization)
import Slacklinker.Linear.Types (LinearBearerToken, unLinearBearerToken)
import Slacklinker.Prelude

newtype GraphqlApp m a = GraphqlApp {unApp :: GraphQLQueryT m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO, MonadGraphQLQuery)

type role GraphqlApp representational nominal

runQuery :: (MonadIO m) => LinearBearerToken -> GraphqlApp m a -> m a
runQuery linearToken app = runGraphQLQueryT (graphqlSettings linearToken.unLinearBearerToken) (unApp app)

-- XXX(jadel): partial failure is kind of busted here
resultThrow :: (MonadIO m) => GraphQLResult a -> m a
resultThrow GraphQLResult {resultErrors = [], resultResult = Just r} = pure r
resultThrow GraphQLResult {resultErrors} = throwIO $ GraphQLException resultErrors

graphqlSettings :: Text -> GraphQLSettings
graphqlSettings secret =
  defaultGraphQLSettings
    { url = "https://api.linear.app/graphql"
    , modifyReq = \req ->
        req
          { requestHeaders =
              (hAuthorization, encodeUtf8 secret) : requestHeaders req
          }
    }
