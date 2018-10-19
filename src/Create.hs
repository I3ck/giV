module Create
  ( createChangeRgxs
  , createChangeRules
  , createFallbacks
  ) where

import           Types
import           Utils

import           Data.List (find)

--------------------------------------------------------------------------------

createChangeRgxs :: Cfg -> ChangeRgxs
createChangeRgxs cfg = ChangeRgxs
  (Regexp <$> majorregexp    cfg)
  (Regexp <$> minorregexp    cfg)
  (Regexp <$> patchregexp    cfg)
  (Regexp <$> nochangeregexp cfg)
  (           tagversioning  cfg)

--------------------------------------------------------------------------------

createChangeRules :: Cfg -> [ChangeRule]
createChangeRules = fmap (\x -> ChangeRule (Regexp $ nameregexp x) (read $ defaultchange x)) . defaultchangerules --TODO dont use read

--------------------------------------------------------------------------------

createFallbacks :: Cfg -> CliArgs -> [ChangeRule] -> BranchMaster Change
createFallbacks cfg args changerules = BranchMaster fallbackB fallbackM
  where
    fallbackM   = read $ defaultchangemaster cfg --TODO dont use read
    fallbackB   = case find (\rule -> matches (nregexp rule) (branch args)) changerules of
                  Just r  -> dchange r
                  Nothing -> read $ defaultchangebranch cfg --TODO dont use read

