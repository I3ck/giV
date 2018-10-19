module Create
  ( createCfg
  , createFallbacks
  ) where

import           Types
import           Utils

import           Data.List (find)

--------------------------------------------------------------------------------

createCfg :: CfgRaw -> Cfg
createCfg CfgRaw{..} = Cfg
  { majorr           = Regexp <$> majorregexp
  , minorr           = Regexp <$> minorregexp
  , patchr           = Regexp <$> patchregexp
  , nochanger        = Regexp <$> nochangeregexp
  , tagver           = tagversioning
  , defaultChanges   = BranchMaster (read defaultchangebranch) (read defaultchangemaster) ---TODO don't use read
  , defaultchangerls = changeRules
  }
  where
    changeRules = fmap (\x -> ChangeRule (Regexp $ nameregexp x) (read $ defaultchange x)) $ defaultchangerules --TODO dont use read

--------------------------------------------------------------------------------

createFallbacks :: Cfg -> CliArgs -> [ChangeRule] -> BranchMaster Change
createFallbacks cfg args changerules = BranchMaster fallbackB fallbackM
  where
    fallbackM   = bMaster . defaultChanges $ cfg
    fallbackB   = case find (\rule -> matches (nregexp rule) (branch args)) changerules of
                  Just r  -> dchange r
                  Nothing -> bBranch . defaultChanges $ cfg

