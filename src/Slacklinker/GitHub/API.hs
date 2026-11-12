-- | Minimal GitHub GraphQL client for reading pull request state.
module Slacklinker.GitHub.API (fetchPrState, prStateFromHttp) where

import Data.Aeson (decode, eitherDecode, encode, object, (.=))
import Network.HTTP.Client (Request (..), RequestBody (..), httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Types (hAuthorization, hContentType, hUserAgent, statusCode)
import Slacklinker.App (App (..), HasApp (..))
import Slacklinker.GitHub.API.Types
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

fetchPrState :: (HasApp m, MonadIO m) => GithubToken -> PrRef -> m (Either Text PrState)
fetchPrState (GithubToken token) ref = do
  manager <- getsApp (.manager)
  req0 <- liftIO $ parseRequest "https://api.github.com/graphql"
  let requestHeaders =
        [ (hAuthorization, "Bearer " <> encodeUtf8 token)
        , (hUserAgent, "slacklinker")
        , (hContentType, "application/json")
        ]
      body = object ["query" .= prQuery, "variables" .= object ["owner" .= ref.repo.owner, "name" .= ref.repo.name, "number" .= ref.number]]
      requestBody = RequestBodyLBS $ encode body
      req = req0 {method = "POST", requestHeaders, requestBody}
  resp <- liftIO $ httpLbs req manager
  pure $ prStateFromHttp (statusCode $ responseStatus resp) (responseBody resp)

prStateFromHttp :: Int -> LByteString -> Either Text PrState
prStateFromHttp 200 body = mapLeft pack (eitherDecode body) >>= prStateFromResponse
prStateFromHttp code body = Left $ "GitHub returned HTTP " <> tshow code <> ": " <> maybe (take 200 . decodeUtf8 $ toStrict body) (.message) (decode @GhError body)

prStateFromResponse :: GhResponse -> Either Text PrState
prStateFromResponse resp = case resp.responseData >>= (.repository) >>= (.pullRequest) of
  Nothing -> Left $ "no pull request in response; errors: " <> maybe "none" (decodeUtf8 . toStrict . encode) resp.errors
  Just pr -> Right PrState {status = toStatus pr, review = toReview pr.reviewDecision ((.state) <$> pr.latestOpinionatedReviews.nodes)}
  where
    toStatus pr = case pr.state of
      GhMerged -> PrMerged
      GhClosed -> PrClosed
      GhOpen
        | pr.isInMergeQueue -> PrQueued
        | pr.isDraft -> PrDraft
        | otherwise -> PrOpened

    -- reviewDecision is null when the branch doesn't require reviews, so fall back to the latest review from each writer.
    toReview (Just DecisionApproved) _ = ReviewApproved
    toReview (Just DecisionChangesRequested) _ = ReviewChangesRequested
    toReview (Just DecisionReviewRequired) _ = ReviewNone
    toReview Nothing states
      | StateChangesRequested `elem` states = ReviewChangesRequested
      | StateApproved `elem` states = ReviewApproved
      | otherwise = ReviewNone
