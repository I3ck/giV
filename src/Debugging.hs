module Debugging
  ( makeDebug 
  ) where

import           Types
import           Version

--------------------------------------------------------------------------------

---TODO simplify
makeDebug :: Cfg -> BranchMaster Change -> BranchMaster [Commit] -> BranchMaster [Change] -> DebugInfo
makeDebug Cfg{..} cdefault (BranchMaster commitsB commitsM) (BranchMaster changesB changesM) = DebugInfo
  { dDefault  = cdefault
  , dMajor    = majorregexp
  , dMinor    = minorregexp
  , dPatch    = patchregexp
  , dNoChange = nochangeregexp
  , dLines    = BranchMaster{bBranch = reverse $ makeLine <$> zip3 versionsB changesB commitsB, bMaster = reverse $ makeLine <$> zip3 versionsM changesM commitsM}
  }
  where
    versionsB = if length scannedB >= 1
                then tail scannedB
                else []
    scannedB  = scanl (flip applyChange) versionM changesB
    versionM  = if length versionsM >= 1
                then last versionsM
                else mempty
    versionsM = if length scannedM >= 1 --TODO not null
                then tail scannedM
                else []
    scannedM  = scanl (flip applyChange) mempty changesM

--------------------------------------------------------------------------------

makeLine :: (Version, Change, Commit) -> DebugLine
makeLine (v, ch, co) = DebugLine {dVersion = v, dChange = ch, dCommit = co}
