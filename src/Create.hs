module Create
  ( createCfg
  , createFallbacks
  ) where

import           Types
import           Utils

import           Data.List (find)

--------------------------------------------------------------------------------

createCfg :: CfgRaw -> Either GiVError Cfg
createCfg CfgRaw{..} = do
   dcb <- maybeToEither (InvalidDefaultChangeBranch $ ErrorSource defaultchangebranch) . maybeRead $ defaultchangebranch
   dcm <- maybeToEither (InvalidDefaultChangeMaster $ ErrorSource defaultchangemaster) . maybeRead $ defaultchangemaster
   crs <- changeRules
   pure Cfg
     { majorr           = Regexp <$> majorregexp
     , minorr           = Regexp <$> minorregexp
     , patchr           = Regexp <$> patchregexp
     , nochanger        = Regexp <$> nochangeregexp
     , tagver           = tagversioning
     , defaultChanges   = BranchMaster dcb dcm
     , defaultchangerls = crs
     }
  where
    changeRules = mapM fRules defaultchangerules
    fRules x    = ChangeRule <$> (pure . Regexp . nameregexp $ x) <*> (maybeToEither (InvalidDefaultChange . ErrorSource . defaultchange $ x) . maybeRead $ defaultchange x)

--------------------------------------------------------------------------------

createFallbacks :: Cfg -> CliArgs -> [ChangeRule] -> BranchMaster Change
createFallbacks cfg args changerules = BranchMaster fallbackB fallbackM
  where
    fallbackM   = bMaster . defaultChanges $ cfg
    fallbackB   = case find (\rule -> matches (nregexp rule) (branch args)) changerules of
                  Just r  -> dchange r
                  Nothing -> bBranch . defaultChanges $ cfg

