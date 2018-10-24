module Debugging
  ( makeDebug
  ) where

import           Data.Maybe (fromMaybe)
import           Types
import           Utils
import           Version

--------------------------------------------------------------------------------

makeDebug :: Cfg -> BranchMaster Change -> BranchMaster [Commit] -> BranchMaster [Change] -> DebugInfo
makeDebug Cfg{..} cdefault (BranchMaster commitsB commitsM) (BranchMaster changesB changesM) = DebugInfo
  { dDefault  = cdefault
  , dMajor    = cMajor
  , dMinor    = cMinor
  , dPatch    = cPatch
  , dNoChange = cNoChange
  , dStart    = start
  , dRules    = cDefaultChangerls
  , dLines    = BranchMaster{branch = linesB, master = linesM}
  }
  where
    linesM    = reverse $ makeLine <$> zip3 versionsM changesM commitsM
    linesB    = reverse $ makeLine <$> zip3 versionsB changesB commitsB
    versionsB = ifNotEmpty tail [] scannedB
    scannedB  = scanl (flip applyChange) versionM changesB
    versionM  = ifNotEmpty last mempty versionsM
    versionsM = ifNotEmpty tail [] scannedM
    scannedM  = scanl (flip applyChange) start changesM
    start     = fromMaybe mempty cStart

--------------------------------------------------------------------------------

makeLine :: (Version, Change, Commit) -> DebugLine
makeLine (v, ch, co) = DebugLine {dVersion = v, dChange = ch, dCommit = co}
