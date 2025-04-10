{-# LANGUAGE TypeOperators #-}

module Slacklinker.Linear.GraphQL (runLinearGraphQL, runQueryThrow, LinearGraphQL (..), PageInfo (..), Cursor, paginateQuery) where

import Data.GraphQL (GraphQLQuery (..), GraphQLSettings (..), Object, Schema, runQuery)
import Data.GraphQL.Monad (GraphQLQueryT, MonadGraphQLQuery, defaultGraphQLSettings, runGraphQLQueryT)
import Data.Kind (Type)
import Network.HTTP.Client (Request (..))
import Network.HTTP.Types (hAuthorization)
import Slacklinker.Linear.Types (LinearBearerToken, unLinearBearerToken)
import Slacklinker.Prelude

newtype LinearGraphQL m a = LinearGraphQL {unLinearGraphQL :: GraphQLQueryT m a}
  deriving stock (Functor)
  deriving newtype (Applicative, Monad, MonadIO, MonadGraphQLQuery)

type role LinearGraphQL representational nominal

{- | A renaming of 'runQuery' from @graphql-client@ to describe its exception
behaviour.
-}
runQueryThrow ::
  forall (m :: Type -> Type) query (schema :: Schema). (MonadIO m, MonadGraphQLQuery m, GraphQLQuery query, schema ~ ResultSchema query) => query -> m (Object schema)
runQueryThrow = runQuery

runLinearGraphQL :: (MonadIO m) => LinearBearerToken -> LinearGraphQL m a -> m a
runLinearGraphQL linearToken action = runGraphQLQueryT (graphqlSettings linearToken.unLinearBearerToken) (unLinearGraphQL action)

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

data PageInfo = PageInfo {endCursor :: Maybe Text, hasNextPage :: Bool}
  deriving stock (Show)

type Cursor = Text

-- | Paginates a response, accumulating results into a full list.
paginateQuery ::
  (MonadGraphQLQuery m, Monoid collection) =>
  (Maybe Cursor -> m (PageInfo, collection)) ->
  m collection
paginateQuery fetch =
  go Nothing mempty
  where
    go cursor accumulator = do
      (pageInfo, items) <- fetch cursor
      let accumulator' = accumulator <> items
      if pageInfo.hasNextPage
        then
          go pageInfo.endCursor accumulator'
        else
          pure accumulator'
