module Process
  ( process
  ) where

import           Types
import           Utils
import           Data.List                        (isInfixOf)
import           Data.List.Split                  (splitOn)

--------------------------------------------------------------------------------

breakingSubjects :: [String] --TODO cfg or cmdlinearg
breakingSubjects =
  [ "breaking"
  , "major"
  ]

featureSubjects :: [String] --TODO cfg or cmdlinearg
featureSubjects =
  [ "feature"
  , "minor"
  ]

fixSubjects :: [String] --TODO cfg or cmdlinearg
fixSubjects =
  [ "patch"
  , "fix"
  ]

noChangeSubjects :: [String] --TODO cfg or cmdlinearg
noChangeSubjects =
  [ "nochange"
  , "noversion"
  ]

--------------------------------------------------------------------------------

process :: Change -> Raw -> [Change]
process fallback raw = processCommit fallback <$> commits raw

--------------------------------------------------------------------------------

processCommit :: Change -> Commit -> Change --TODO otherwise must return the user defined default
processCommit fallback c = case tryReadVersion =<< tag c of
  Just to -> SetTo to
  Nothing 
    | isBreaking c -> Breaking
    | isFeature c  -> Feature
    | isFix c      -> Fix
    | isNoChange c -> NoChange
    | otherwise    -> fallback

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

isBreaking :: Commit -> Bool
isBreaking Commit{..} = isBreakingSubject subject

isBreakingSubject :: Subject -> Bool
isBreakingSubject s = any (`isInfixOf` s) breakingSubjects

--------------------------------------------------------------------------------

isFeature :: Commit -> Bool
isFeature Commit{..} = isFeatureSubject subject

isFeatureSubject :: Subject -> Bool
isFeatureSubject s = any (`isInfixOf` s) featureSubjects

--------------------------------------------------------------------------------

isFix :: Commit -> Bool
isFix Commit{..} = isFixSubject subject

isFixSubject :: Subject -> Bool
isFixSubject s = any (`isInfixOf` s) fixSubjects

--------------------------------------------------------------------------------

isNoChange :: Commit -> Bool
isNoChange Commit{..} = isNoChangeSubject subject

isNoChangeSubject :: Subject -> Bool
isNoChangeSubject s = any (`isInfixOf` s) noChangeSubjects
