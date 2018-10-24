module Parse
    ( parseCommitString
    ) where

import           Types

import           Control.Applicative              (optional)
import           Control.Monad.Except (throwError)
import           Data.Text
import           Data.Attoparsec.Text.Lazy as A
import           Data.String.Conversions          (cs)

--------------------------------------------------------------------------------

parseCommitString :: CommitString -> Either GiVError [Commit]
parseCommitString cs = case parse (many' parseCommit) . unCommitString $ cs of
                         (Fail _ _ e) -> throwError . UnableToParseCommitString . pack $ e
                         (Done _ x)   -> pure x

--------------------------------------------------------------------------------

parseCommit :: Parser Commit
parseCommit = do
  skipSpace
  tag <- optional parseTag
  char '*'
  message <- parseMessage
  skipWhile (== '\0')
  pure Commit{tag = tag, message = message}

--------------------------------------------------------------------------------

parseTag :: Parser Tag
parseTag = do
  string "(tag:"
  skipSpace
  tag <- takeWhile1 (/= ')')
  char ')'
  skipSpace
  pure $ Tag tag

--------------------------------------------------------------------------------

parseMessage :: Parser Message
parseMessage = Message . cs <$> restTillNull

--------------------------------------------------------------------------------

restTillNull :: Parser Text
restTillNull = option "" $ takeTill (== '\0')
