module Create
  ( createCfg
  , createFallbacks
  ) where

import           Types
import           Utils

import           Data.List (find)
import           Data.Text (unpack)

--------------------------------------------------------------------------------

createCfg :: CfgRaw -> Either GiVError Cfg
createCfg CfgRaw{..} = do
   dcb <- maybeToEither (InvalidDefaultChangeBranch $ ErrorSource defaultchangebranch) . maybeRead . unpack $ defaultchangebranch
   dcm <- maybeToEither (InvalidDefaultChangeMaster $ ErrorSource defaultchangemaster) . maybeRead . unpack $ defaultchangemaster
   crs <- changeRules
   pure Cfg
     { cMajor           = Regexp <$> majorregexp
     , cMinor           = Regexp <$> minorregexp
     , cPatch           = Regexp <$> patchregexp
     , cNoChange        = Regexp <$> nochangeregexp
     , cTagVer          = tagversioning
     , cDefaultChanges   = BranchMaster dcb dcm
     , cDefaultChangerls = crs
     }
  where
    changeRules = mapM fRules defaultchangerules
    fRules x    = ChangeRule <$> (maybeToEither (InvalidDefaultChange . ErrorSource . defaultchange $ x) . maybeRead . unpack $ defaultchange x) <*> (pure . Regexp . nameregexp $ x)

--------------------------------------------------------------------------------

createFallbacks :: Cfg -> CliArgs -> [ChangeRule] -> BranchMaster Change
createFallbacks cfg args changerules = BranchMaster fallbackB fallbackM
  where
    fallbackM   = master . cDefaultChanges $ cfg
    fallbackB   = case find (\cr -> matches (rule cr) (aBranch args)) changerules of
                  Just r  -> change r
                  Nothing -> branch . cDefaultChanges $ cfg
