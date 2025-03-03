-- | Extracts things that look like Linear ticket IDs from strings.
module Slacklinker.Linear.Extract (LinearTicketId (..), extractTicketIds) where

import Control.Monad (MonadFail (..))
import Data.Attoparsec.Text
import Data.Char (isAsciiUpper)
import Data.Either (fromRight)
import Data.Set qualified as Set
import Slacklinker.Linear.Types (LinearTicketId (..))
import Slacklinker.Prelude hiding (takeWhile, try)

isWordBoundaryChar :: Char -> Bool
isWordBoundaryChar = (`Set.member` Set.fromList " ,.!()[]/")

-- | Asserts that the character at the cursor is a word boundary
wordBoundaryEndCheck :: Parser ()
wordBoundaryEndCheck =
  peekChar >>= \case
    Just c -> unless (isWordBoundaryChar c) $ fail "ticket not ending on a word boundary"
    -- EOI is fine
    Nothing -> pure ()

isLinearTicketSlugChar :: Char -> Bool
isLinearTicketSlugChar = isAsciiUpper

-- | Parses something that looks like a Linear ticket slug out of some free text.
linearTicketId :: Parser LinearTicketId
linearTicketId = LinearTicketId <$> takeWhile isLinearTicketSlugChar <* char '-' <*> decimal <* wordBoundaryEndCheck

-- | Extracts the ticket IDs that appear in a string.
extractTicketIds :: Text -> [LinearTicketId]
extractTicketIds =
  -- XXX(jadel): this is a really funny way of writing this; it's Like This
  -- because it has to be able to fail parsing something and then ignore it,
  -- and doing that inside Applicative is kind of a pain.
  fromRight [] . parseOnly do
    firstId <- tryTicketId <|> consumeSingleChar
    maybe identity cons firstId <$> ticketIdsParser
  where
    -- 1. Start of input: ticket ID at the very start will not have boundaries.
    tryTicketId = Just <$> try linearTicketId
    -- 2. After the start, ticket IDs will appear after word boundaries
    skipTillWordBoundary = skipWhile (not . isWordBoundaryChar)
    skipWordBoundaries = skipWhile isWordBoundaryChar
    consumeSingleChar = anyChar $> Nothing
    -- 3. If there is not a ticket there, yield Nothing for that possible
    --    position and keep going.
    ticketIdOrNothing = skipTillWordBoundary *> skipWordBoundaries *> (tryTicketId <|> consumeSingleChar)
    -- 4. Do it as long as there is text.
    ticketIdsParser = catMaybes <$> many ticketIdOrNothing
