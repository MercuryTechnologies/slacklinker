module Slacklinker.Extract.Parse (
  extractUrls,
  extractRawLinks,
  parseUrl,
) where

import Control.Applicative (pure, (*>), (<*))
import Control.Monad (void)
import Data.Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Function (($), (.))
import Data.List (concatMap)
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T
import Data.Tuple (snd)
import Data.Vector qualified as V
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void Text

parseUrl :: Text -> Parser Text
parseUrl subdomain = try $ do
  void $ string "https://"
  void $ string subdomain
  void $ string ".slack.com/archives/"
  channelId <- some (alphaNumChar <|> char '-')
  void $ char '/'
  void $ char 'p'
  timestamp <- some digitChar
  pure $ T.concat ["https://", subdomain, ".slack.com/archives/", T.pack channelId, "/p", T.pack timestamp]

extractUrls :: Text -> Text -> [Text]
extractUrls subdomain input = fromMaybe [] $ parseMaybe urlsParser input
  where
    -- fix subdomain
    parseUrl' :: Parser Text
    parseUrl' = parseUrl subdomain

    -- Parse a list of URLs
    urlsParser :: Parser [Text]
    urlsParser = many (notUrl *> parseUrl' <* notUrl)

    -- Consume any non-URL characters (zero or more)
    notUrl :: Parser ()
    notUrl = skipMany $ notFollowedBy parseUrl' *> anySingle

-- Recursively extract strings from JSON and apply URL extraction
extractRawLinks :: Text -> Value -> [Text]
extractRawLinks slackSubdomain (Object obj) = concatMap (extractRawLinks slackSubdomain . snd) (KeyMap.toList obj)
extractRawLinks slackSubdomain (Array arr) = concatMap (extractRawLinks slackSubdomain) (V.toList arr)
extractRawLinks slackSubdomain (String str) = extractUrls slackSubdomain str
extractRawLinks _ _ = []
