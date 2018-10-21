module Version
  ( version
  , applyChange
  ) where

import           Types
import           Instances ()

--------------------------------------------------------------------------------

version :: Maybe Version -> BranchMaster [Change] -> Version
version mstart (BranchMaster csB csM) = foldl (flip applyChange) vM csB
  where
    startversion = case mstart of
                     Nothing -> mempty
                     Just x  -> x
    vM = foldl (flip applyChange) startversion csM

--------------------------------------------------------------------------------

applyChange :: Change -> Version -> Version
applyChange NoChange v  = v{                                                                     vcount = vcount v + 1}
applyChange Fix v       = v{                                              vpatch = vpatch v + 1, vcount = 0}
applyChange Feature v   = v{                       vminor = vminor v + 1, vpatch = 0,            vcount = 0}
applyChange Breaking v  = v{vmajor = vmajor v + 1, vminor = 0,            vpatch = 0,            vcount = 0}
applyChange (SetTo x) _ = x
