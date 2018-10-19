module Types where

import GHC.Generics
import Control.Monad.Except

--------------------------------------------------------------------------------

type GiV = ExceptT GiVError IO

--------------------------------------------------------------------------------

newtype Subject = Subject
  { unSubject :: String
  }

--------------------------------------------------------------------------------

newtype Tag = Tag
  { unTag :: String
  }

--------------------------------------------------------------------------------

newtype Branch = Branch
  { unBranch :: String
  }

--------------------------------------------------------------------------------

newtype CommitString = CommitString
  { unCommitString :: String
  }

--------------------------------------------------------------------------------

newtype Regexp = Regexp
 { unRegexp :: String
 }

--------------------------------------------------------------------------------

newtype ErrorSource = ErrorSource
  { unErrorSource :: String
  }

--------------------------------------------------------------------------------

data GiVError
  = YamlDecodeError            String
  | InvalidDefaultChangeBranch ErrorSource
  | InvalidDefaultChangeMaster ErrorSource
  | InvalidDefaultChange       ErrorSource
  | UnableToParseCommitString  String

--------------------------------------------------------------------------------

data BranchMaster a = BranchMaster
  { branch :: a
  , master :: a
  }

--------------------------------------------------------------------------------

data CliArgs = CliArgs
  { aRepo    :: String
  , aCfg     :: String
  , aBranch  :: String
  , aVerbose :: Bool
  }

--------------------------------------------------------------------------------

data ChangeRule = ChangeRule
  { change :: Change
  , rule   :: Regexp
  }

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
  { cMajor            :: Maybe Regexp
  , cMinor            :: Maybe Regexp
  , cPatch            :: Maybe Regexp
  , cNoChange         :: Maybe Regexp
  , cTagVer           :: Bool
  , cDefaultChanges   :: BranchMaster Change
  , cDefaultChangerls :: [ChangeRule]
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
  }

--------------------------------------------------------------------------------

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  } deriving (Read, Eq, Ord)
