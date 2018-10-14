module Parse
    ( parseCommitString
    ) where

import           Types

import           Control.Applicative              (optional)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as DText
import           Data.String.Conversions          (cs)

--------------------------------------------------------------------------------

parseCommitString :: CommitString -> Either String [Commit]
parseCommitString = parseOnly (parseCommits <* endOfInput) . cs . unCommitString

--------------------------------------------------------------------------------

parseCommits :: Parser [Commit]
parseCommits = do
  many' parseCommit

--------------------------------------------------------------------------------

parseCommit :: Parser Commit
parseCommit = do
  skipSpace
  tag <- optional parseTag
  char '|'
  subject <- parseSubject
  skipSpace
  pure Commit{tag = tag, subject = subject}

--------------------------------------------------------------------------------

parseTag :: Parser Tag
parseTag = do
  char '('
  tag <- takeWhile1 (/= ')')
  char ')'
  skipSpace
  pure . Tag . cs $ tag

--------------------------------------------------------------------------------

parseSubject :: Parser Subject
parseSubject = Subject . cs <$> restOfLine

--------------------------------------------------------------------------------

restOfLine :: Parser DText.ByteString
restOfLine = option "" $ takeTill (== '\n')
