module Process
  ( process
  ) where

import           Types
import           Utils
import           Data.Text (splitOn, unpack)

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
  let vSplits = splitOn "v" t --TODO pass "v" via cfg / cmdline
  dotSplits <- if length vSplits == 2
               then pure $ splitOn "." $ vSplits !! 1
               else Nothing
  if length dotSplits == 3
  then Version <$> maybeRead (unpack $ dotSplits !! 0) <*> maybeRead (unpack $ dotSplits !! 1) <*> maybeRead (unpack $ dotSplits !! 2) <*> pure 0
  else Nothing

--------------------------------------------------------------------------------

cMatches :: Maybe Regexp -> Commit -> Bool
cMatches mrgx Commit{..} = maybe False (`subjectMatches` subject) mrgx

subjectMatches :: Regexp -> Subject -> Bool
subjectMatches rgx (Subject subj) = matches rgx subj

