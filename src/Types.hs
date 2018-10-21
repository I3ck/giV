module Types where

import Data.Text
import GHC.Generics
import Control.Monad.Except

--------------------------------------------------------------------------------

type GiV = ExceptT GiVError IO

--------------------------------------------------------------------------------

newtype Subject = Subject
  { unSubject :: Text
  }

--------------------------------------------------------------------------------

newtype Tag = Tag
  { unTag :: Text
  }

--------------------------------------------------------------------------------

newtype Branch = Branch
  { unBranch :: Text
  }

--------------------------------------------------------------------------------

newtype CommitString = CommitString
  { unCommitString :: Text
  }

--------------------------------------------------------------------------------

newtype Regexp = Regexp
 { unRegexp :: Text
 }

--------------------------------------------------------------------------------

newtype ErrorSource = ErrorSource
  { unErrorSource :: Text
  }

--------------------------------------------------------------------------------

data GiVError
  = YamlDecodeError            Text
  | InvalidDefaultChangeBranch ErrorSource
  | InvalidDefaultChangeMaster ErrorSource
  | InvalidDefaultChange       ErrorSource
  | UnableToParseCommitString  Text
  | InvalidOutputFormat        ErrorSource

--------------------------------------------------------------------------------

data BranchMaster a = BranchMaster
  { branch :: a
  , master :: a
  }

--------------------------------------------------------------------------------

data OutputFormat
  = OutputVersion
  | OutputYAML
  | OutputJSON
  deriving (Read)

--------------------------------------------------------------------------------

data ArgsRaw = ArgsRaw
  { arRepo    :: Text
  , arCfg     :: Text
  , arBranch  :: Text
  , arOutput  :: Text
  , arVerbose :: Bool
  }

data Args = Args
  { aRepo    :: Text
  , aCfg     :: Text
  , aBranch  :: Text
  , aOutput  :: OutputFormat
  , aVerbose :: Bool
  }

--------------------------------------------------------------------------------

data ChangeRule = ChangeRule
  { change :: Change
  , rule   :: Regexp
  }

data ChangeRuleRaw = ChangeRuleRaw
  { nameregexp    :: Text
  , defaultchange :: Text
  } deriving (Generic)

--------------------------------------------------------------------------------

data CfgRaw = CfgRaw
  { majorregexp          :: Maybe Text
  , minorregexp          :: Maybe Text
  , patchregexp          :: Maybe Text
  , nochangeregexp       :: Maybe Text
  , startversion         :: Maybe Text
  , tagversioning        :: Bool
  , defaultchangemaster  :: Text
  , defaultchangebranch  :: Text
  , defaultchangerules   :: [ChangeRuleRaw]
  } deriving (Generic)

data Cfg = Cfg
  { cMajor            :: Maybe Regexp
  , cMinor            :: Maybe Regexp
  , cPatch            :: Maybe Regexp
  , cNoChange         :: Maybe Regexp
  , cStart            :: Maybe Version
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
  , dStart    :: Version
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
  { vmajor :: Int
  , vminor :: Int
  , vpatch :: Int
  , vcount :: Int
  } deriving (Read, Eq, Ord)

--------------------------------------------------------------------------------

data Result = Result
  { major :: Int
  , minor :: Int
  , patch :: Int
  , count :: Int
  } deriving (Generic)
