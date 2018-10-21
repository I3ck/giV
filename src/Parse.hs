module Parse
    ( parseCommitString
    ) where

import           Types

import           Control.Applicative              (optional)
import           Control.Monad.Except (throwError)
import           Data.Text
import           Data.Attoparsec.Text
import           Data.String.Conversions          (cs)

--------------------------------------------------------------------------------

parseCommitString :: CommitString -> Either GiVError [Commit]
parseCommitString cs = case parseCommitString' cs of
                         Left e  -> throwError . UnableToParseCommitString . pack $ e
                         Right x -> pure x

--------------------------------------------------------------------------------

parseCommitString' :: CommitString -> Either String [Commit]
parseCommitString' = parseOnly (parseCommits) . unCommitString

--------------------------------------------------------------------------------

parseCommits :: Parser [Commit]
parseCommits = many' parseCommit

--------------------------------------------------------------------------------

parseCommit :: Parser Commit
parseCommit = do
  skipSpace
  tag <- optional parseTag
  char '|'
  subject <- parseSubject
  skipWhile (== '\0')
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

restOfLine :: Parser Text
restOfLine = option "" $ takeTill (== '\0')
