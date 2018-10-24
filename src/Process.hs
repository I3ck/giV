module Process
  ( process
  , tryReadVersion
  ) where

import           Types
import           Utils
import qualified Data.Text as T

--------------------------------------------------------------------------------

process :: Cfg -> Change -> [Commit] -> [Change]
process cfg fallback cs = processCommit cfg fallback <$> cs

--------------------------------------------------------------------------------

processCommit :: Cfg -> Change -> Commit -> Change
processCommit Cfg{..} fallback c
  = case sTo of
    Just to -> SetTo to
    Nothing
      | cMatches cMajor c    -> Breaking
      | cMatches cMinor c    -> Feature
      | cMatches cPatch c    -> Fix
      | cMatches cNoChange c -> NoChange
      | otherwise            -> fallback
    where
      sTo | not cTagVer = Nothing
          | otherwise   = tryReadVersion =<< tag c

--------------------------------------------------------------------------------

tryReadVersion :: Tag -> Maybe Version
tryReadVersion (Tag t) = do
  let vSplits = T.splitOn "v" t --TODO pass "v" via cfg / cmdline
  dotSplits <- if (length vSplits == 2) && (T.null . head $ vSplits)
               then pure $ T.splitOn "." $ vSplits !! 1
               else Nothing
  if length dotSplits == 3
  then Version <$> maybeRead (T.unpack $ dotSplits !! 0) <*> maybeRead (T.unpack $ dotSplits !! 1) <*> maybeRead (T.unpack $ dotSplits !! 2) <*> pure 0
  else Nothing

--------------------------------------------------------------------------------

cMatches :: Maybe Regexp -> Commit -> Bool
cMatches mrgx Commit{..} = maybe False (`messageMatches` message) mrgx

messageMatches :: Regexp -> Message -> Bool
messageMatches rgx (Message msg) = matches rgx msg

