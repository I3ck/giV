module Process
  ( process
  ) where

import           Types
import           Utils
import           Data.List                        (isInfixOf)
import           Data.List.Split                  (splitOn)

--------------------------------------------------------------------------------

process :: ChangeWords -> Change -> Raw -> [Change]
process changewords fallback raw = processCommit changewords fallback <$> commits raw

--------------------------------------------------------------------------------

processCommit :: ChangeWords -> Change -> Commit -> Change
processCommit ChangeWords{..} fallback c
  = case tryReadVersion =<< tag c of
    Just to -> SetTo to
    Nothing
      | containsWord majorw c    -> Breaking
      | containsWord minorw c    -> Feature
      | containsWord patchw c    -> Fix
      | containsWord nochangew c -> NoChange
      | otherwise                -> fallback

--------------------------------------------------------------------------------

tryReadVersion :: Tag -> Maybe Version
tryReadVersion t = do
  let vSplits = splitOn "v" t --TODO pass "v" via cfg / cmdline
  dotSplits <- if length vSplits == 2
               then pure $ splitOn "." $ vSplits !! 1
               else Nothing
  if length dotSplits == 3
  then Version <$> (maybeRead $ dotSplits !! 0) <*> (maybeRead $ dotSplits !! 1) <*> (maybeRead $ dotSplits !! 2)
  else Nothing

--------------------------------------------------------------------------------

containsWord :: String -> Commit -> Bool
containsWord str Commit{..} = subjectContainsWord str subject

subjectContainsWord :: String -> Subject -> Bool
subjectContainsWord = isInfixOf

