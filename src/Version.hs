module Version
  ( version
  , semVerOf
  , applyChange
  ) where

import           Types
import           Utils
import           Instances  ()
import qualified Data.Text  as T
import qualified Data.Maybe as M

--------------------------------------------------------------------------------

semVerOf :: Label -> Version -> SemVer
semVerOf (Label l) Version{..} = SemVer . T.pack $
     (show vmajor)
  ++ "."
  ++ (show vminor)
  ++ "."
  ++ (show vpatch)
  ++ "-"
  ++ T.unpack l
  ++ "+"
  ++ (show vcount)

--------------------------------------------------------------------------------

version :: Maybe Version -> BranchMaster [Change] -> Version
version mstart (BranchMaster csB csM) = foldl (flip applyChange) vM csBIgnored
  where
    startversion = M.fromMaybe mempty mstart
    vM = foldl (flip applyChange) startversion (ignoreFirstIncrement csM)

    csBIgnored | null csM = ignoreFirstIncrement csB
               | otherwise = csB

--------------------------------------------------------------------------------

applyChange :: Change -> Version -> Version
applyChange NoChange v  = v{                                                                     vcount = vcount v + 1}
applyChange Fix v       = v{                                              vpatch = vpatch v + 1, vcount = 0}
applyChange Feature v   = v{                       vminor = vminor v + 1, vpatch = 0,            vcount = 0}
applyChange Breaking v  = v{vmajor = vmajor v + 1, vminor = 0,            vpatch = 0,            vcount = 0}
applyChange (SetTo x) _ = x

