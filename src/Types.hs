module Types where

import GHC.Generics

--------------------------------------------------------------------------------

newtype Subject = Subject
  { unSubject :: String
  } deriving (Show)

--------------------------------------------------------------------------------

newtype Tag = Tag
  { unTag :: String
  } deriving (Show)

--------------------------------------------------------------------------------

newtype Branch = Branch
  { unBranch :: String
  } deriving (Show)

--------------------------------------------------------------------------------

newtype CommitString = CommitString
  { unCommitString :: String
  } deriving (Show)

--------------------------------------------------------------------------------

newtype Regexp = Regexp
 { unRegexp :: String
 }

--------------------------------------------------------------------------------

data BranchMaster a = BranchMaster
  { bBranch :: a
  , bMaster :: a
  } deriving (Show)

--------------------------------------------------------------------------------

data CliArgs = CliArgs
  { repo    :: String
  , cfg     :: String
  , branch  :: String
  , verbose :: Bool
  }

--------------------------------------------------------------------------------

data ChangeRule = ChangeRule
  { nregexp :: Regexp
  , dchange :: Change
  } deriving (Generic)

data ChangeRuleRaw = ChangeRuleRaw
  { nameregexp    :: String
  , defaultchange :: String
  } deriving (Generic)

--------------------------------------------------------------------------------

data CfgRaw = CfgRaw
  { majorregexp          :: Maybe String
  , minorregexp          :: Maybe String
  , patchregexp          :: Maybe String
  , nochangeregexp       :: Maybe String
  , tagversioning        :: Bool
  , defaultchangemaster  :: String
  , defaultchangebranch  :: String
  , defaultchangerules   :: [ChangeRuleRaw]
  } deriving (Generic)

data Cfg = Cfg
  { majorr           :: Maybe Regexp
  , minorr           :: Maybe Regexp
  , patchr           :: Maybe Regexp
  , nochanger        :: Maybe Regexp
  , tagver           :: Bool
  , defaultChanges   :: BranchMaster Change
  , defaultchangerls :: [ChangeRule]
  }

--------------------------------------------------------------------------------

data DebugInfo = DebugInfo
  { dDefault  :: BranchMaster Change
  , dRules    :: [ChangeRule]
  , dMajor    :: Maybe Regexp
  , dMinor    :: Maybe Regexp
  , dPatch    :: Maybe Regexp
  , dNoChange :: Maybe Regexp
  , dLines    :: BranchMaster [DebugLine]
  }

--------------------------------------------------------------------------------

data DebugLine = DebugLine
  { dVersion :: Version
  , dChange  :: Change
  , dCommit  :: Commit
  }

--------------------------------------------------------------------------------

data Change
  = NoChange
  | Fix
  | Feature
  | Breaking
  | SetTo Version
  deriving (Read)

deriving instance Show Version => Show Change

--------------------------------------------------------------------------------

data Commit = Commit
  { tag     :: Maybe Tag
  , subject :: Subject
  } deriving (Show)

--------------------------------------------------------------------------------

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  }
  deriving (Read, Eq, Ord)
