{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( parseCommitString
    ) where

import           Types

import           Data.Attoparsec.ByteString.Char8
--import qualified Data.ByteString.Char8            as DText
import           Data.String.Conversions          (cs)

--------------------------------------------------------------------------------

parseCommitString :: CommitString -> Either String Raw
parseCommitString = parseOnly (parseRaw <* endOfInput) . cs . unCommitString

--------------------------------------------------------------------------------

parseRaw :: Parser Raw
parseRaw = do
  commits <- many1 parseCommit
  pure Raw {rCommits = commits}

--------------------------------------------------------------------------------

parseCommit :: Parser Commit ---TODO currently incorrect
parseCommit = do
  tag      <- parseTag
  subject  <- parseSubject
  skipSpace
  pure Commit {cTag = tag, cSubject = subject}

--------------------------------------------------------------------------------

parseTag :: Parser (Maybe Tag)
parseTag = undefined --TODO

--------------------------------------------------------------------------------

parseSubject :: Parser Subject
parseSubject = undefined --TODO

--------------------------------------------------------------------------------
{- TODO remove if stays unused
restOfLine :: Parser DText.ByteString
restOfLine = option "" $ takeTill (== '\n')
-}
