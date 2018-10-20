module Version
  ( version
  , applyChange
  ) where

import           Types
import           Instances ()

--------------------------------------------------------------------------------

version :: BranchMaster [Change] -> Version
version (BranchMaster csB csM) = foldl (flip applyChange) vM csB
  where
    vM = foldl (flip applyChange) mempty csM

--------------------------------------------------------------------------------

applyChange :: Change -> Version -> Version
applyChange NoChange v  = v
applyChange Fix v       = v{                                              vpatch = vpatch v + 1}
applyChange Feature v   = v{                       vminor = vminor v + 1, vpatch = 0}
applyChange Breaking v  = v{vmajor = vmajor v + 1, vminor = 0,            vpatch = 0}
applyChange (SetTo x) _ = x
