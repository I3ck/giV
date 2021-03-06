module Instances where

import qualified Data.Text as T
import qualified Data.Yaml as Y
import           Types
import           Utils

--------------------------------------------------------------------------------

instance Show Version where
  show Version{..} = show vmajor ++ "." ++ show vminor ++ "." ++ show vpatch ++ ":" ++ show vcount

instance Semigroup Version where
  v1 <> v2 = if v1 > v2
             then v1
             else v2

instance Monoid Version where
  mempty = Version { vmajor = 0, vminor = 0, vpatch = 0, vcount = 0}

--------------------------------------------------------------------------------

instance Y.FromJSON ChangeRuleRaw where

--------------------------------------------------------------------------------

instance Y.FromJSON CfgRaw where

--------------------------------------------------------------------------------

instance Y.ToJSON CommitHash where

--------------------------------------------------------------------------------

instance Y.ToJSON Result where

--------------------------------------------------------------------------------

instance Functor BranchMaster where
  fmap f (BranchMaster x y) = BranchMaster (f x) (f y)

instance Applicative BranchMaster where
  pure x                                      = BranchMaster x x
  (BranchMaster fx fy) <*> (BranchMaster x y) = BranchMaster (fx x) (fy y)

--------------------------------------------------------------------------------

instance Show ChangeRule where
  show ChangeRule{..} = show change ++ " <- " ++ (T.unpack . unRegexp $ rule)

--------------------------------------------------------------------------------

instance Show DebugInfo where
  show DebugInfo{..} = unlines
    [ "Default change branch: " ++ (show . branch $ dDefault)
    , "Default change master: " ++ (show . master $ dDefault)
    , "Major change word: " ++ mShow dMajor
    , "Minor change word: " ++ mShow dMinor
    , "Patch change word: " ++ mShow dPatch
    , "No change word: "    ++ mShow dNoChange
    , "Start version: " ++ show dStart
    , "Default change rules: "
    ]
    ++ '\n' : (unlines . fmap show $ dRules)
    ++ (if not . null . branch $ dLines then "\nBRANCH CHANGES\n" else "")
    ++ (unlines . fmap show . branch $ dLines)
    ++ (if not . null . master $ dLines then "\nMASTER CHANGES\n" else "")
    ++ (unlines . fmap show . master $ dLines)
    where
      mShow Nothing  = "NOT SET"
      mShow (Just x) = T.unpack . unRegexp $ x

--------------------------------------------------------------------------------

instance Show DebugLine where
  show DebugLine{..} = show dVersion ++ " -> " ++ showCh dChange ++ " " ++ (indentedNewLines 4 . showCo $ dCommit)
    where
      showCh NoChange  = "[NONE] "
      showCh Fix       = "[FIX]  "
      showCh Feature   = "[FEAT] "
      showCh Breaking  = "[BREAK]"
      showCh (SetTo _) = "[SET]  "

      showCo c = showCo' (tag c) (message c)

      showCo' Nothing m  =                                       T.unpack . unMessage $ m
      showCo' (Just t) m = "(" ++ (show . unTag $ t) ++ ") " ++ (T.unpack . unMessage $ m)

--------------------------------------------------------------------------------

instance Show GiVError where
  show (YamlDecodeError s)             = "Unable to decode .yaml file: " ++ T.unpack s
  show (InvalidDefaultChangeBranch es) = "The default change given for a branch '" ++ (T.unpack . unErrorSource $ es) ++ "' is invalid"
  show (InvalidDefaultChangeMaster es) = "The default change given for master '" ++ (T.unpack . unErrorSource $ es) ++ "' is invalid"
  show (InvalidDefaultChange es)       = "The default change '" ++ (T.unpack . unErrorSource $ es) ++ "' is invalid"
  show (UnableToParseCommitString s)   = "Unable to parse commit string: " ++ T.unpack s
  show (InvalidOutputFormat es)        = "The provided OutputFormat '" ++ (T.unpack . unErrorSource $ es) ++ "' is invalid"

