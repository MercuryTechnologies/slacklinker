-- | Extracts GitHub pull request references from message text.
module Slacklinker.GitHub.Extract (extractPrRefs) where

import Data.Attoparsec.Text hiding (number)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Either.Combinators (rightToMaybe)
import Data.Text qualified as T
import Slacklinker.GitHub.Types (PrRef (..), mkRepoName)
import Slacklinker.Prelude hiding (takeWhile)

isOwnerChar :: Char -> Bool
isOwnerChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '-'

isRepoChar :: Char -> Bool
isRepoChar c = isOwnerChar c || c == '_' || c == '.'

prRefP :: Parser PrRef
prRefP = do
  owner <- takeWhile1 isOwnerChar
  name <- char '/' *> takeWhile1 isRepoChar
  number <- string "/pull/" *> decimal
  pure PrRef {repo = mkRepoName owner name, number}

extractPrRefs :: Text -> [PrRef]
extractPrRefs text = ordNub $ mapMaybe parsePiece candidates
  where
    pieces = T.splitOn "github.com/" (T.toLower text)
    candidates = [p | (prev, p) <- zip pieces (drop 1 pieces), any (`T.isSuffixOf` prev) ["://", "://www."]]
    parsePiece = rightToMaybe . parseOnly prRefP
