module Version
  ( version
  ) where

import Types

--------------------------------------------------------------------------------

startVersion :: Version --TODO pass via args?
startVersion = Version { major = 0, minor = 0, patch = 1}

--------------------------------------------------------------------------------

version :: [Change] -> Version
version = foldl (\version change -> applyChange change version) startVersion

--------------------------------------------------------------------------------

applyChange :: Change -> Version -> Version
applyChange NoChange v = v
applyChange Fix v      = v{                                          patch = patch v + 1}
applyChange Feature v  = v{                     minor = minor v + 1, patch = 0}
applyChange Breaking v = v{major = major v + 1, minor = 0,           patch = 0}