module Instances where

import           Types
import           Data.Yaml
import           Data.Text (unpack)

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

instance FromJSON ChangeRuleRaw where

--------------------------------------------------------------------------------

instance FromJSON CfgRaw where

--------------------------------------------------------------------------------

instance Functor BranchMaster where
  fmap f (BranchMaster x y) = BranchMaster (f x) (f y)

instance Applicative BranchMaster where
  pure x                                      = BranchMaster x x
  (BranchMaster fx fy) <*> (BranchMaster x y) = BranchMaster (fx x) (fy y)

--------------------------------------------------------------------------------

instance Show ChangeRule where
  show ChangeRule{..} = show change ++ " <- " ++ (unpack . unRegexp $ rule)

--------------------------------------------------------------------------------

instance Show DebugInfo where
  show DebugInfo{..} = unlines
    [ "Default change branch: " ++ (show . branch $ dDefault)
    , "Default change master: " ++ (show . master $ dDefault)
    , "Major change word: " ++ mShow dMajor
    , "Minor change word: " ++ mShow dMinor
    , "Patch change word: " ++ mShow dPatch
    , "No change word: "    ++ mShow dNoChange
    , "Default change rules: "
    ]
    ++ '\n' : (unlines . fmap show $ dRules)
    ++ (if not . null . branch $ dLines then "\nBRANCH CHANGES\n" else "")
    ++ (unlines . fmap show . branch $ dLines)
    ++ (if not . null . master $ dLines then "\nMASTER CHANGES\n" else "")
    ++ (unlines . fmap show . master $ dLines)
    where
      mShow Nothing  = "NOT SET"
      mShow (Just x) = unpack . unRegexp $ x

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

      showCo' Nothing s  = unpack . unSubject $ s
      showCo' (Just t) s = (unpack . unSubject $ s) ++ " [" ++ (show . unTag $ t) ++ "]"

--------------------------------------------------------------------------------

instance Show GiVError where
  show (YamlDecodeError s)             = "Unable to decode .yaml file: " ++ unpack s
  show (InvalidDefaultChangeBranch es) = "The default change given for a branch '" ++ (unpack . unErrorSource $ es) ++ "' is invalid"
  show (InvalidDefaultChangeMaster es) = "The default change given for master '" ++ (unpack . unErrorSource $ es) ++ "' is invalid"
  show (InvalidDefaultChange es)       = "The default change '" ++ (unpack . unErrorSource $ es) ++ "' is invalid"
  show (UnableToParseCommitString s)   = "Unable to parse commit string: " ++ unpack s

