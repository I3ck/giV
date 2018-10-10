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

processCommit :: ChangeWords -> Change -> Commit -> Change --TODO otherwise must return the user defined default
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
  let splits = splitOn "v" t --TODO pass "v" via cfg / cmdline 
  vSplits <- if length splits == 2
             then pure $ splitOn "." $ splits !! 1
             else Nothing
  if length vSplits == 3
  then Version <$> (maybeRead $ vSplits !! 0) <*> (maybeRead $ vSplits !! 1) <*> (maybeRead $ vSplits !! 2)
  else Nothing

--------------------------------------------------------------------------------

containsWord :: String -> Commit -> Bool
containsWord str Commit{..} = subjectContainsWord str subject

subjectContainsWord :: String -> Subject -> Bool
subjectContainsWord = isInfixOf

