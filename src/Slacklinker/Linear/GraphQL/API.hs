{- This file was automatically generated and should not be edited. -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -w #-}

module Slacklinker.Linear.GraphQL.API where

import Data.GraphQL
import Data.GraphQL.Bootstrap
import Slacklinker.Linear.GraphQL.Scalars

{-----------------------------------------------------------------------------
-- LinkSlack

-- result :: Object LinkSlackSchema; throws a GraphQL exception on errors
result <- runQuery LinkSlackMutation
  { _issueId = ...
  , _slackLink = ...
  }

-- result :: GraphQLResult (Object LinkSlackSchema)
result <- runQuerySafe LinkSlackMutation
  { _issueId = ...
  , _slackLink = ...
  }
-----------------------------------------------------------------------------}

data LinkSlackMutation = LinkSlackMutation
  { _issueId :: Text
  , _slackLink :: Text
  }
  deriving (Show)

type LinkSlackSchema =
  [schema|
  {
    attachmentLinkSlack: {
      success: Bool,
      attachment: {
        issue: {
          id: Text,
        },
      },
    },
  }
|]

instance GraphQLQuery LinkSlackMutation where
  type ResultSchema LinkSlackMutation = LinkSlackSchema

  getQueryName _ = "LinkSlack"

  getQueryText _ =
    [query|
    mutation LinkSlack($issueId: String!, $slackLink: String!) {
      attachmentLinkSlack(issueId: $issueId, url: $slackLink) {
        success
        attachment {
          issue {
            id
          }
        }
      }
    }
  |]

  getArgs LinkSlackMutation {..} =
    object
      [ "issueId" .= _issueId
      , "slackLink" .= _slackLink
      ]

{-----------------------------------------------------------------------------
-- ListTeams

-- result :: Object ListTeamsSchema; throws a GraphQL exception on errors
result <- runQuery ListTeamsQuery
  { _startCursor = ...
  }

-- result :: GraphQLResult (Object ListTeamsSchema)
result <- runQuerySafe ListTeamsQuery
  { _startCursor = ...
  }
-----------------------------------------------------------------------------}

data ListTeamsQuery = ListTeamsQuery
  { _startCursor :: Maybe Text
  }
  deriving (Show)

type ListTeamsSchema =
  [schema|
  {
    organization: {
      urlKey: Text,
    },
    teams: {
      nodes: List {
        name: Text,
        key: Text,
      },
      pageInfo: {
        endCursor: Maybe Text,
        hasNextPage: Bool,
      },
    },
  }
|]

instance GraphQLQuery ListTeamsQuery where
  type ResultSchema ListTeamsQuery = ListTeamsSchema

  getQueryName _ = "ListTeams"

  getQueryText _ =
    [query|
    query ListTeams($startCursor: String) {
      organization {
        urlKey
      }
      teams(after: $startCursor, first: 100, includeArchived: false) {
        nodes {
          name
          key
        }
        pageInfo {
          endCursor
          hasNextPage
        }
      }
    }
  |]

  getArgs ListTeamsQuery {..} =
    object
      [ "startCursor" .= _startCursor
      ]

{-----------------------------------------------------------------------------
-- WorkspaceInfo

-- result :: Object WorkspaceInfoSchema; throws a GraphQL exception on errors
result <- runQuery WorkspaceInfoQuery
  {
  }

-- result :: GraphQLResult (Object WorkspaceInfoSchema)
result <- runQuerySafe WorkspaceInfoQuery
  {
  }
-----------------------------------------------------------------------------}

data WorkspaceInfoQuery = WorkspaceInfoQuery
  {
  }
  deriving (Show)

type WorkspaceInfoSchema =
  [schema|
  {
    organization: {
      urlKey: Text,
      name: Text,
      id: Text,
    },
  }
|]

instance GraphQLQuery WorkspaceInfoQuery where
  type ResultSchema WorkspaceInfoQuery = WorkspaceInfoSchema

  getQueryName _ = "WorkspaceInfo"

  getQueryText _ =
    [query|
    query WorkspaceInfo {
      organization {
        urlKey
        name
        id
      }
    }
  |]

  getArgs WorkspaceInfoQuery {} =
    object
      []
