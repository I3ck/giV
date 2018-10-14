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

data Cfg = Cfg
  { defaultchangeMaster  :: String
  , defaultchangeBranch  :: String
  , majorregexp          :: Maybe String
  , minorregexp          :: Maybe String
  , patchregexp          :: Maybe String
  , nochangeregexp       :: Maybe String
  , tagversioning        :: Bool
  } deriving (Generic)

--------------------------------------------------------------------------------

data DebugInfo = DebugInfo
  { dDefault  :: BranchMaster Change
  , dMajor    :: Maybe String
  , dMinor    :: Maybe String
  , dPatch    :: Maybe String
  , dNoChange :: Maybe String
  , dLines    :: BranchMaster [DebugLine]
  }

--------------------------------------------------------------------------------

data DebugLine = DebugLine
  { dVersion :: Version
  , dChange  :: Change
  , dCommit  :: Commit
  }

--------------------------------------------------------------------------------

data ChangeRgxs = ChangeRgxs ---TODO rename or split
  { majorrgx    :: Maybe Regexp
  , minorrgx    :: Maybe Regexp
  , patchrgx    :: Maybe Regexp
  , nochangergx :: Maybe Regexp
  , tagvs       :: Bool
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
