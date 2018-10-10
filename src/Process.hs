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

noChangeSubjects :: [String] --TODO cfg or cmdlinearg
noChangeSubjects =
  [ "nochange"
  , "noversion"
  ]

--------------------------------------------------------------------------------

process :: Raw -> [Change]
process raw = processCommit <$> commits raw

--------------------------------------------------------------------------------

processCommit :: Commit -> Change
processCommit c | isBreaking c = Breaking
                | isFeature c  = Feature
                | isNoChange c = NoChange
                | otherwise    = case tryReadVersion =<< tag c of
                                   Nothing -> Fix
                                   Just to -> SetTo to

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

isNoChange :: Commit -> Bool
isNoChange Commit{..} = isNoChangeSubject subject

isNoChangeSubject :: Subject -> Bool
isNoChangeSubject s = any (`isInfixOf` s) noChangeSubjects
