module Process
  ( process
  ) where

import           Types
import           Data.List                        (isInfixOf)

--------------------------------------------------------------------------------

breakingTags :: [String] --TODO cfg or cmdlinearg
breakingTags =
  [ "breaking"
  , "major"
  ]

breakingSubjects :: [String] --TODO cfg or cmdlinearg
breakingSubjects =
  [ "breaking"
  , "major"
  ]

featureTags :: [String] --TODO cfg or cmdlinearg
featureTags =
  [ "feature"
  , "minor"
  ]

featureSubjects :: [String] --TODO cfg or cmdlinearg
featureSubjects =
  [ "feature"
  , "minor"
  ]

noChangeTags :: [String] --TODO cfg or cmdlinearg
noChangeTags =
  [ "nochange"
  , "noversion"
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
                | otherwise    = Fix

--------------------------------------------------------------------------------

isBreaking :: Commit -> Bool
isBreaking Commit{..} = isBreakingSubject subject || tagBreaking
  where
    tagBreaking = maybe False isBreakingTag tag

isBreakingTag :: Tag -> Bool
isBreakingTag s = any (`isInfixOf` s) breakingTags

isBreakingSubject :: Subject -> Bool
isBreakingSubject s = any (`isInfixOf` s) breakingSubjects

--------------------------------------------------------------------------------

isFeature :: Commit -> Bool
isFeature Commit{..} = isFeatureSubject subject || tagFeature
  where
    tagFeature = maybe False isFeatureTag tag

isFeatureTag :: Tag -> Bool
isFeatureTag s = any (`isInfixOf` s) featureTags

isFeatureSubject :: Subject -> Bool
isFeatureSubject s = any (`isInfixOf` s) featureSubjects

--------------------------------------------------------------------------------

isNoChange :: Commit -> Bool
isNoChange Commit{..} = isNoChangeSubject subject || tagNoChange
  where
    tagNoChange = maybe False isNoChangeTag tag

isNoChangeTag :: Tag -> Bool
isNoChangeTag s = any (`isInfixOf` s) noChangeTags

isNoChangeSubject :: Subject -> Bool
isNoChangeSubject s = any (`isInfixOf` s) noChangeSubjects
