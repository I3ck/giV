{-# LANGUAGE OverloadedStrings #-}

module Parse
    ( parseCommitString
    ) where

import           Types

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as DText
import           Data.String.Conversions          (cs)
import           Control.Applicative              (optional)

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
  mtag     <- optional parseTag
  char '|'
  subject  <- parseSubject
  skipSpace
  pure Commit {cTag = mtag, cSubject = subject}

--------------------------------------------------------------------------------

parseTag :: Parser Tag
parseTag = do
  string "(tag:"
  skipSpace
  tag <- takeWhile1 (/= ')')
  char ')'
  skipSpace
  pure . cs $ tag

--------------------------------------------------------------------------------

parseSubject :: Parser Subject
parseSubject = cs <$> restOfLine 

--------------------------------------------------------------------------------

restOfLine :: Parser DText.ByteString
restOfLine = option "" $ takeTill (== '\n')
