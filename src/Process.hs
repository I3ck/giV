module Process
  ( process
  ) where

import           Types
import           Utils
import           Data.List.Split                  (splitOn)

--------------------------------------------------------------------------------

process :: ChangeRgxs -> Change -> [Commit] -> [Change]
process changergxs fallback cs = processCommit changergxs fallback <$> cs

--------------------------------------------------------------------------------

processCommit :: ChangeRgxs -> Change -> Commit -> Change
processCommit ChangeRgxs{..} fallback c
  = case sTo of
    Just to -> SetTo to
    Nothing
      | cMatches majorrgx c    -> Breaking
      | cMatches minorrgx c    -> Feature
      | cMatches patchrgx c    -> Fix
      | cMatches nochangergx c -> NoChange
      | otherwise              -> fallback
    where
      sTo | not tagvs = Nothing
          | otherwise = tryReadVersion =<< tag c

--------------------------------------------------------------------------------

tryReadVersion :: Tag -> Maybe Version
tryReadVersion (Tag t) = do
  let vSplits = splitOn "v" t --TODO pass "v" via cfg / cmdline
  dotSplits <- if length vSplits == 2
               then pure $ splitOn "." $ vSplits !! 1
               else Nothing
  if length dotSplits == 3
  then Version <$> maybeRead (dotSplits !! 0) <*> maybeRead (dotSplits !! 1) <*> maybeRead (dotSplits !! 2)
  else Nothing

--------------------------------------------------------------------------------

cMatches :: Maybe Regexp -> Commit -> Bool
cMatches mrgx Commit{..} = maybe False (`subjectMatches` subject) mrgx

subjectMatches :: Regexp -> Subject -> Bool
subjectMatches rgx (Subject subj) = matches rgx subj

