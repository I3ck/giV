module Process
  ( process
  ) where

import           Types
import           Data.List                        (isInfixOf)

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
                | otherwise    = case setToOf c of
                                   Nothing -> Fix
                                   Just to -> SetTo to

--------------------------------------------------------------------------------

setToOf :: Commit -> Maybe Version
setToOf _ = undefined ---TODO

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
