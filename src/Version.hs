module Version
  ( version
  , applyChange
  ) where

import           Types
import           Instances ()

--------------------------------------------------------------------------------

version :: [Change] -> Version
version = foldl (flip applyChange) mempty

--------------------------------------------------------------------------------

applyChange :: Change -> Version -> Version
applyChange NoChange v  = v
applyChange Fix v       = v{                                          patch = patch v + 1}
applyChange Feature v   = v{                     minor = minor v + 1, patch = 0}
applyChange Breaking v  = v{major = major v + 1, minor = 0,           patch = 0}
applyChange (SetTo x) _ = x
