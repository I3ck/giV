module Debugging
  ( makeDebug 
  ) where

import           Types
import           Version

--------------------------------------------------------------------------------

makeDebug :: Cfg -> Change -> [Commit] -> [Change] -> DebugInfo
makeDebug Cfg{..} cdefault commits changes = DebugInfo
  { dDefault  = cdefault
  , dMajor    = majorregexp
  , dMinor    = minorregexp
  , dPatch    = patchregexp
  , dNoChange = nochangeregexp
  , dLines    = reverse $ makeLine <$> zip3 versions changes commits
  }
  where
    versions = if length scanned >= 1
               then tail scanned
               else []
    scanned  = scanl (flip applyChange) mempty changes

--------------------------------------------------------------------------------

makeLine :: (Version, Change, Commit) -> DebugLine
makeLine (v, ch, co) = DebugLine {dVersion = v, dChange = ch, dCommit = co}
