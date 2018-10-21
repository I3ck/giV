module Result
  ( createResult
  , asVersion
  ) where

import Types

--------------------------------------------------------------------------------

createResult :: Version -> Result
createResult Version{..} = Result
  { major = vmajor
  , minor = vminor
  , patch = vpatch
  , count = vcount
  }

--------------------------------------------------------------------------------

asVersion :: Result -> String --TODO Text?
asVersion Result{..} = show major ++ "." ++ show minor ++ "." ++ show patch

