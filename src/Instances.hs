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

--------------------------------------------------------------------------------

instance Show DebugInfo where
  show DebugInfo{..} = unlines
    [ "Default change: " ++ show dDefault
    , "Major change word: " ++ mShow dMajor
    , "Minor change word: " ++ mShow dMinor
    , "Patch change word: " ++ mShow dPatch
    , "No change word: "    ++ mShow dNoChange
    ]
    ++ '\n' : (unlines . fmap show $ dLines)
    where
      mShow Nothing  = "NOT SET"
      mShow (Just x) = x


--------------------------------------------------------------------------------

instance Show DebugLine where
  show DebugLine{..} = show dVersion ++ " -> " ++ showCh dChange ++ " " ++ showCo dCommit 
    where
      showCh NoChange  = "[  N  ]"
      showCh Fix       = "[ FIX ]"
      showCh Feature   = "[FEAT ]"
      showCh Breaking  = "[BREAK]"
      showCh (SetTo _) = "[ SET ]"

      showCo c = showCo' (tag c) (subject c)

      showCo' Nothing s  = unSubject s
      showCo' (Just t) s = unSubject s ++ " [" ++ (show . unTag $ t) ++ "]"

