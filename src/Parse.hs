module Parse
    ( parseCommitString
    ) where

import           Types

import           Control.Applicative              (optional)
import           Control.Monad.Except             (throwError)
import qualified Data.Text                        as T
import           Data.Attoparsec.Text.Lazy        as A
import qualified Data.String.Conversions          as CV

--------------------------------------------------------------------------------

parseCommitString :: CommitString -> Either GiVError [CommitRaw]
parseCommitString cs = case parse (many' parseCommit <* endOfInput) . unCommitString $ cs of
                         (Fail _ _ e) -> throwError . UnableToParseCommitString . T.pack $ e
                         (Done _ x)   -> pure x

--------------------------------------------------------------------------------

parseCommit :: Parser CommitRaw
parseCommit = do
  skipSpace
  refs <- optional parseRefs
  char '*'
  message <- parseMessage
  skipWhile (== '\0')
  pure CommitRaw{refs = refs, rmessage = message}

--------------------------------------------------------------------------------

parseRefs :: Parser [Ref]
parseRefs = many1 $ do
  skipSpace
  ref <- takeWhile1 (\c -> c /= ',' && c /= '*')
  optional $ char ','
  pure . Ref $ ref

--------------------------------------------------------------------------------

parseMessage :: Parser Message
parseMessage = Message . CV.cs <$> restTillNull

--------------------------------------------------------------------------------

restTillNull :: Parser T.Text
restTillNull = option "" $ takeTill (== '\0')
