{-# LANGUAGE RecordWildCards #-}

module Process
  ( process
  ) where

import Types

import Data.List (isInfixOf)

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
process raw = processCommit <$> rCommits raw

--------------------------------------------------------------------------------

processCommit :: Commit -> Change
processCommit c | isBreaking c = Breaking
                | isFeature c  = Feature
                | isNoChange c = NoChange
                | otherwise    = Fix

--------------------------------------------------------------------------------

isBreaking :: Commit -> Bool
isBreaking Commit{..} = isBreakingSubject cSubject || tagBreaking
  where
    tagBreaking = maybe False isBreakingTag cTag

isBreakingTag :: Tag -> Bool
isBreakingTag s = any (\x -> x `isInfixOf` s) breakingTags

isBreakingSubject :: Subject -> Bool
isBreakingSubject s = any (\x -> x `isInfixOf` s) breakingSubjects

--------------------------------------------------------------------------------

isFeature :: Commit -> Bool
isFeature Commit{..} = isFeatureSubject cSubject || tagFeature
  where
    tagFeature = maybe False isFeatureTag cTag

isFeatureTag :: Tag -> Bool
isFeatureTag s = any (\x -> x `isInfixOf` s) featureTags

isFeatureSubject :: Subject -> Bool
isFeatureSubject s = any (\x -> x `isInfixOf` s) featureSubjects

--------------------------------------------------------------------------------

isNoChange :: Commit -> Bool
isNoChange Commit{..} = isNoChangeSubject cSubject || tagNoChange
  where
    tagNoChange = maybe False isNoChangeTag cTag

isNoChangeTag :: Tag -> Bool
isNoChangeTag s = any (\x -> x `isInfixOf` s) noChangeTags

isNoChangeSubject :: Subject -> Bool
isNoChangeSubject s = any (\x -> x `isInfixOf` s) noChangeSubjects


