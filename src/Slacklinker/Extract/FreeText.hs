module Slacklinker.Extract.FreeText (
  extractUrls,
  extractLinksFromJson,
) where

import Control.Applicative (many, pure, (*>), (<$>), (<|>))
import Control.Monad (fail, unless)
import Data.Aeson (Value (Array, Object, String))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Attoparsec.Text (Parser, anyChar, parseOnly, skipWhile, takeWhile, try)
import Data.Bool (Bool, not, (||))
import Data.Char (Char, isAlphaNum)
import Data.Either (either, fromRight)
import Data.Eq ((==))
import Data.Function (const, ($), (.))
import Data.List (concatMap, elem)
import Data.Maybe (Maybe (..), catMaybes, maybe)
import Data.String (String)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Tuple (snd)
import Data.Vector qualified as V
import Slacklinker.SplitUrl (SlackUrlParts (..), buildSlackUrl, getUriSlackParts)
import URI.ByteString (parseURI, strictURIParserOptions)

-- According to RFC 3986
isUriChar :: Char -> Bool
isUriChar c = isAlphaNum c || c `elem` ("-._~:/?#[]@!$&'()*+,;=%" :: String)

-- https://datatracker.ietf.org/doc/html/rfc3986#section-3.1
--       scheme      = ALPHA *( ALPHA / DIGIT / "+" / "-" / "." )
-- technically should be checking that it starts with alpha, but, close enough
isUriSchemeChar :: Char -> Bool
isUriSchemeChar c = isAlphaNum c || c `elem` ("+-." :: String)

parseUrl :: Text -> Parser Text
parseUrl subdomain = do
  input <- takeWhile isUriChar
  uri <- either (const $ fail "URI parse failed") pure $ parseURI strictURIParserOptions (encodeUtf8 input)
  parts <- maybe (fail "URI does not match Slack URL pattern") pure $ getUriSlackParts uri
  unless (subdomain == parts.workspaceName) $ fail "Workspace name does not match subdomain"
  maybe (fail "Failed to build Slack URL") pure $ buildSlackUrl parts

extractUrls :: Text -> Text -> [Text]
extractUrls subdomain = fromRight [] . parseOnly urlsParser
  where
    -- 1. skip any non-scheme characters
    skipNonSchemeChars = skipWhile (not . isUriSchemeChar)
    -- 2. try to parse a URL, if it succeeds consume & wrap in Just
    tryParsingUrl = Just <$> try (parseUrl subdomain)
    -- 3. if it fails, consume a single char and return Nothing
    consumeSingleChar = anyChar *> pure Nothing
    urlOrNothing = skipNonSchemeChars *> (tryParsingUrl <|> consumeSingleChar)
    -- 4. repeat many times
    -- 5. at the end, keep only Justs (successful parses)
    urlsParser = catMaybes <$> many urlOrNothing

extractLinksFromJson :: Text -> Value -> [Text]
extractLinksFromJson slackSubdomain (Object obj) = concatMap (extractLinksFromJson slackSubdomain . snd) (KeyMap.toList obj)
extractLinksFromJson slackSubdomain (Array arr) = concatMap (extractLinksFromJson slackSubdomain) (V.toList arr)
extractLinksFromJson slackSubdomain (String str) = extractUrls slackSubdomain str
extractLinksFromJson _ _ = []
