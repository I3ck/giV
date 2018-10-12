module Instances where

import           Types
import           Data.Yaml

--------------------------------------------------------------------------------

instance Show Version where
  show Version{..} = show major ++ "." ++ show minor ++ "." ++ show patch

instance Semigroup Version where
  v1 <> v2 = if v1 > v2 
             then v1
             else v2

instance Monoid Version where
  mempty = Version { major = 0, minor = 0, patch = 0}

--------------------------------------------------------------------------------

instance FromJSON Cfg where
