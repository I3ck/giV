module Result
  ( createResult
  , asVersion
  ) where

import Types

--------------------------------------------------------------------------------

createResult :: SemVer -> Version -> CommitHash -> Result
createResult (SemVer s) Version{..} hash = Result
  { major      = vmajor
  , minor      = vminor
  , patch      = vpatch
  , count      = vcount
  , semver     = s
  , commithash = unCommitHash hash
  }

--------------------------------------------------------------------------------

asVersion :: Result -> String --TODO Text?
asVersion Result{..} = show major ++ "." ++ show minor ++ "." ++ show patch

