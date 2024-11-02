module Slacklinker.SplitUrl where

import Data.Char (isDigit)
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
  { workspaceName :: Text
  , channelId :: ConversationId
  , messageTs :: Text
  , threadTs :: Maybe Text
  }
  deriving stock (Show)

buildSlackUrl :: SlackUrlParts -> Maybe Text
buildSlackUrl SlackUrlParts {..} = do
  messageTsP <- messageTsToP messageTs
  pure $ "https://" <> workspaceName <> ".slack.com/archives/" <> channelId.unConversationId <> "/" <> messageTsP <> threadTsPart
  where
    -- due to mysterious jackage at slack dot com, you need to put the channel ID
    -- twice, or else the previewer will not work properly. No idea why.
    -- (11/01/2024): today, the previewer is a lot more lenient. keeping this anyway, as slack itself copies links with cid
    threadTsPart = maybe "" (\tts -> "?thread_ts=" <> tts <> "&cid=" <> channelId.unConversationId) threadTs

isSlack :: Host -> Bool
isSlack (Host host) = "slack.com" `isSuffixOf` host

getSlackSubdomain :: Host -> Maybe Text
getSlackSubdomain (Host host) = stripSuffix ".slack.com" (decodeUtf8 host)

{- | Splits a Slack URL into bits

>>> splitSlackUrl "https://jadeapptesting.slack.com/archives/C043YJGBY49/p1663961111250399"
Just (SlackUrlParts {channelId = ConversationId {unConversationId = "C043YJGBY49"}, messageTs = "1663961111.250399", threadTs = Nothing})

>>> splitSlackUrl "https://jadeapptesting.slack.com/archives/C043YJGBY49/p1663980532340209?thread_ts=1663966375.232459&cid=C043YJGBY49"
Just (SlackUrlParts {channelId = ConversationId {unConversationId = "C043YJGBY49"}, messageTs = "1663980532.340209", threadTs = Just "1663966375.232459"})
-}
splitSlackUrl :: Text -> Maybe SlackUrlParts
splitSlackUrl url = do
  Right uri <- pure $ parseURI strictURIParserOptions (cs url)
  getUriSlackParts uri

getUriSlackParts :: URI -> Maybe SlackUrlParts
getUriSlackParts (URI {uriPath, uriAuthority, uriQuery = Query {queryPairs}}) = do
  host <- authorityHost <$> uriAuthority
  workspaceName <- getSlackSubdomain host
  [_slash, "archives", channelId_, messageTs_] <- pure $ T.split (== '/') (trimEndings $ cs uriPath)
  let messageTs = pToMessageTs messageTs_
      threadTs = trimEndings . cs <$> List.lookup "thread_ts" queryPairs
      channelId = ConversationId channelId_
  guard $ validateTs messageTs
  guard $ maybe True validateTs threadTs
  Just SlackUrlParts {..}

-- | Validates a Slack timestamp (ts)
-- A valid ts must be either:
-- 1. Exactly 10 digits, followed by a dot, followed by 6 digits
-- 2. A 'p' followed by exactly 16 digits
validateTs :: Text -> Bool
validateTs ts
    | T.null ts = False
    | T.head ts == 'p' = T.length rest == 16 && T.all isDigit rest
    | T.length before /= 10 = False
    | T.length after /= 7 = False -- .123456 is 7 characters
    | not (T.all isDigit before) = False
    | not (T.all isDigit (T.tail after)) = False -- skip the dot
    | otherwise = True
  where
    rest = T.tail ts
    (before, after) = T.breakOn "." ts

-- it would be reasonable to write in github
-- > i am writing this pr to fix a nasty bug (https://myteam.slack.com/archives/C05JCSKNE2G/p1729711459290519) that occured in prod
-- turns out that () are valid characters in URLs, so the URL parser returns https://myteam.slack.com/archives/C05JCSKNE2G/p1729711459290519)
-- for this super common use case we will just strip ending punctuation like ),;!. from the end of paths & query strings 
-- instead of building a more complicated parser
trimEndings :: Text -> Text
trimEndings = T.dropWhileEnd (`T.elem` "),;!.")

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
