module Slacklinker.SplitUrl where

import Data.List qualified as List
import Data.Text qualified as T
import Slacklinker.Import
import URI.ByteString
import Web.Slack.Common (ConversationId (..))

{- |
Slack message URLs are constructed of three parts:

> https://jadeapptesting.slack.com/archives/C043YJGBY49/p1663980532340209
>     ?thread_ts=1663966375.232459&cid=C043YJGBY49

 * Conversation (interchangeably used with "channel" throughout Slack
   schemas), here, "C043YJGBY49"
 * "ts", which looks suspiciously like a high-precision Unix timestamp with
   the decimal point stripped and a \"p\" prepended. In this structure, the
   @ts@ is represented with a decimal point, in the same format as
   @thread_ts@.
 * "thread_ts", which denotes the *parent* of the message in a thread. All
   messages in a thread share the same thread_ts. If there's no @thread_ts@,
   the message is directly in a channel.
-}
data SlackUrlParts = SlackUrlParts
  { channelId :: ConversationId
  , messageTs :: Text
  , threadTs :: Maybe Text
  }
  deriving stock (Show)

buildSlackUrl :: Text -> SlackUrlParts -> Maybe Text
buildSlackUrl workspaceName SlackUrlParts {..} = do
  messageTsP <- messageTsToP messageTs
  pure $ "https://" <> workspaceName <> ".slack.com/archives/" <> channelId.unConversationId <> "/" <> messageTsP <> threadTsPart
  where
    -- due to mysterious jackage at slack dot com, you need to put the channel ID
    -- twice, or else the previewer will not work properly. No idea why.
    threadTsPart = maybe "" (\tts -> "?thread_ts=" <> tts <> "&cid=" <> channelId.unConversationId) threadTs

isSlack :: Host -> Bool
isSlack (Host host) = "slack.com" `isSuffixOf` host

{- | Splits a Slack URL into bits

>>> splitSlackUrl "https://jadeapptesting.slack.com/archives/C043YJGBY49/p1663961111250399"
Just (SlackUrlParts {channelId = ConversationId {unConversationId = "C043YJGBY49"}, messageTs = "1663961111.250399", threadTs = Nothing})

>>> splitSlackUrl "https://jadeapptesting.slack.com/archives/C043YJGBY49/p1663980532340209?thread_ts=1663966375.232459&cid=C043YJGBY49"
Just (SlackUrlParts {channelId = ConversationId {unConversationId = "C043YJGBY49"}, messageTs = "1663980532.340209", threadTs = Just "1663966375.232459"})
-}
splitSlackUrl :: Text -> Maybe SlackUrlParts
splitSlackUrl url = do
  Right (URI {uriPath, uriAuthority, uriQuery = Query {queryPairs}}) <- pure $ parseURI strictURIParserOptions (cs url)
  host <- authorityHost <$> uriAuthority
  guard $ isSlack host

  [_slash, "archives", channelId_, messageTs_] <- pure $ T.split (== '/') (cs uriPath)
  let messageTs = pToMessageTs messageTs_
  let threadTs = cs <$> List.lookup "thread_ts" queryPairs
  let channelId = ConversationId channelId_

  pure SlackUrlParts {..}

{- | Turns a ts into the pxxxx argument you see in URLs

>>> messageTsToP "1663980532.340209"
Just "p1663980532340209"
-}
messageTsToP :: Text -> Maybe Text
messageTsToP ts = do
  [a, b] <- pure $ T.split (== '.') ts
  pure $ "p" <> a <> b

{- | Turns p1663980532340209 into a ts that you can give to slack

>>> pToMessageTs "p1663980532340209"
"1663980532.340209"
-}
pToMessageTs :: Text -> Text
pToMessageTs ts =
  let unprefixed = T.drop 1 ts
      (beforePoint, afterPoint) = splitAtDecimalPoint unprefixed
   in beforePoint <> "." <> afterPoint
  where
    splitAtDecimalPoint s = T.splitAt (T.length s - 6) s
