module Result
  ( createResult
  , asVersion
  ) where

import Types

--------------------------------------------------------------------------------

createResult :: Version -> CommitHash -> Result
createResult Version{..} hash = Result
  { major      = vmajor
  , minor      = vminor
  , patch      = vpatch
  , count      = vcount
  , commithash = unCommitHash hash
  }

--------------------------------------------------------------------------------

asVersion :: Result -> String --TODO Text?
asVersion Result{..} = show major ++ "." ++ show minor ++ "." ++ show patch

