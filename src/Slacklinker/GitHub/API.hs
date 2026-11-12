-- | Minimal GitHub GraphQL client for reading pull request state.
module Slacklinker.GitHub.API (fetchPrState, prStateFromHttp, prStateFromResponse) where

import Control.Monad.Fail (fail)
import Data.Aeson (Value, decode, eitherDecode, encode, object, (.:), (.:?), (.=))
import Data.Aeson.Types (Parser, parseEither, parseMaybe)
import Network.HTTP.Client (Request (..), RequestBody (..), httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types (hAuthorization, hContentType, hUserAgent, statusCode)
import Slacklinker.App (App (..), HasApp (..))
import Slacklinker.GitHub.Types
import Slacklinker.Import

prQuery :: Text
prQuery =
  unlines
    [ "query($owner: String!, $name: String!, $number: Int!) {"
    , "  repository(owner: $owner, name: $name) {"
    , "    pullRequest(number: $number) {"
    , "      state isDraft isInMergeQueue reviewDecision"
    , "      latestOpinionatedReviews(first: 100, writersOnly: true) { nodes { state } }"
    , "    }"
    , "  }"
    , "}"
    ]

-- | Fetches the current state of a pull request.
fetchPrState :: (HasApp m, MonadIO m) => GithubToken -> PrRef -> m (Either Text PrState)
fetchPrState (GithubToken token) ref = do
  manager <- getsApp (.manager)
  req0 <- liftIO $ parseRequest "https://api.github.com/graphql"
  let body =
        object
          [ "query" .= prQuery
          , "variables" .= object ["owner" .= ref.owner, "name" .= ref.repo, "number" .= ref.number]
          ]
      req =
        req0
          { method = "POST"
          , requestHeaders =
              [ (hAuthorization, "Bearer " <> encodeUtf8 token)
              , (hUserAgent, "slacklinker")
              , (hContentType, "application/json")
              ]
          , requestBody = RequestBodyLBS (encode body)
          }
  resp <- liftIO $ httpLbs req manager
  pure $ prStateFromHttp (statusCode $ responseStatus resp) (responseBody resp)

-- | Interprets an HTTP response to 'prQuery'.
prStateFromHttp :: Int -> LByteString -> Either Text PrState
prStateFromHttp 200 body = mapLeft pack (eitherDecode body) >>= prStateFromResponse
prStateFromHttp code body = Left $ "GitHub returned HTTP " <> tshow code <> ": " <> fromMaybe (take 200 . decodeUtf8 $ toStrict body) message
  where
    message = decode body >>= parseMaybe (withObject "error" (.: "message"))

-- | Interprets a GraphQL response to 'prQuery'.
prStateFromResponse :: Value -> Either Text PrState
prStateFromResponse = mapLeft pack . parseEither parser
  where
    parser :: Value -> Parser PrState
    parser = withObject "response" \o -> do
      mData <- o .:? "data"
      mPr <- case mData of
        Nothing -> pure Nothing
        Just d -> do
          mRepo <- d .:? "repository"
          maybe (pure Nothing) (.:? "pullRequest") mRepo
      case mPr of
        Nothing -> do
          errs <- o .:? "errors" :: Parser (Maybe Value)
          fail $ "no pull request in response; errors: " <> maybe "none" show errs
        Just pr -> do
          state <- pr .: "state"
          isDraft <- pr .: "isDraft"
          isInMergeQueue <- pr .: "isInMergeQueue"
          reviewDecision <- pr .:? "reviewDecision"
          reviews <- pr .: "latestOpinionatedReviews" >>= (.: "nodes") >>= traverse (.: "state")
          status <- case state :: Text of
            "MERGED" -> pure PrMerged
            "CLOSED" -> pure PrClosed
            "OPEN"
              | isInMergeQueue -> pure PrQueued
              | isDraft -> pure PrDraft
              | otherwise -> pure PrOpened
            other -> fail $ "unknown pull request state " <> unpack other
          pure PrState {status, review = toReview reviewDecision reviews}

    -- reviewDecision is null when the branch doesn't require reviews, so fall
    -- back to the latest review from each writer.
    toReview :: Maybe Text -> [Text] -> PrReview
    toReview (Just "APPROVED") _ = ReviewApproved
    toReview (Just "CHANGES_REQUESTED") _ = ReviewChangesRequested
    toReview (Just _) _ = ReviewNone
    toReview Nothing reviews
      | "CHANGES_REQUESTED" `elem` reviews = ReviewChangesRequested
      | "APPROVED" `elem` reviews = ReviewApproved
      | otherwise = ReviewNone
