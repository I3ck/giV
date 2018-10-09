module Parse
    ( parseCommitString
    ) where

import           Types

import           Control.Applicative              (optional)
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8            as DText
import           Data.String.Conversions          (cs)

--------------------------------------------------------------------------------

parseCommitString :: CommitString -> Either String Raw
parseCommitString = parseOnly (parseRaw <* endOfInput) . cs . unCommitString

--------------------------------------------------------------------------------

parseRaw :: Parser Raw
parseRaw = do
  commits <- many1 parseCommit
  pure Raw{commits = commits}

--------------------------------------------------------------------------------

parseCommit :: Parser Commit ---TODO currently incorrect
parseCommit = do
  skipSpace
  tag     <- optional parseTag
  char '|'
  subject  <- parseSubject
  skipSpace
  pure Commit{tag = tag, subject = subject}

--------------------------------------------------------------------------------

--TODO this version currently reads all tags and not just (tag: foo)
--TODO consider changing Parser or filtering afterwards
parseTag :: Parser Tag
parseTag = do
  --string "(tag:"
  char '('
  --skipSpace
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
