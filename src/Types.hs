module Types where

import qualified Data.Text      as T
import qualified Data.Text.Lazy as TL
import           GHC.Generics
import           Control.Monad.Except

--------------------------------------------------------------------------------

type GiV = ExceptT GiVError IO

--------------------------------------------------------------------------------

newtype Message = Message
  { unMessage :: T.Text
  } deriving (Show, Eq)

--------------------------------------------------------------------------------

newtype Tag = Tag
  { unTag :: T.Text
  } deriving (Show, Eq)

--------------------------------------------------------------------------------

newtype Ref = Ref
  { unRef :: T.Text
  } deriving (Show, Eq)

--------------------------------------------------------------------------------

newtype Branch = Branch
  { unBranch :: T.Text
  }

--------------------------------------------------------------------------------

newtype CommitString = CommitString
  { unCommitString :: TL.Text
  }

--------------------------------------------------------------------------------

newtype CommitHash = CommitHash
  { unCommitHash :: T.Text
  } deriving (Generic)

--------------------------------------------------------------------------------

newtype Label = Label
  { unLabel :: T.Text
  }

--------------------------------------------------------------------------------

newtype SemVer = SemVer
  { unSemVer :: T.Text
  }

--------------------------------------------------------------------------------

newtype Regexp = Regexp
 { unRegexp :: T.Text
 }

--------------------------------------------------------------------------------

newtype ErrorSource = ErrorSource
  { unErrorSource :: T.Text
  } deriving (Eq)

--------------------------------------------------------------------------------

data GiVError
  = YamlDecodeError            T.Text
  | InvalidDefaultChangeBranch ErrorSource
  | InvalidDefaultChangeMaster ErrorSource
  | InvalidDefaultChange       ErrorSource
  | UnableToParseCommitString  T.Text
  | InvalidOutputFormat        ErrorSource
  deriving (Eq)

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
  { arRepo    :: T.Text
  , arCfg     :: T.Text
  , arBranch  :: T.Text
  , arLabel   :: T.Text
  , arOutput  :: T.Text
  , arVerbose :: Bool
  }

data Args = Args
  { aRepo    :: T.Text
  , aCfg     :: T.Text
  , aBranch  :: T.Text
  , aLabel   :: Label
  , aOutput  :: OutputFormat
  , aVerbose :: Bool
  }

--------------------------------------------------------------------------------

data ChangeRule = ChangeRule
  { change :: Change
  , rule   :: Regexp
  }

data ChangeRuleRaw = ChangeRuleRaw
  { nameregexp    :: T.Text
  , defaultchange :: T.Text
  } deriving (Generic)

--------------------------------------------------------------------------------

data CfgRaw = CfgRaw
  { majorregexp          :: Maybe T.Text
  , minorregexp          :: Maybe T.Text
  , patchregexp          :: Maybe T.Text
  , nochangeregexp       :: Maybe T.Text
  , startversion         :: Maybe T.Text
  , tagversioning        :: Bool
  , defaultchangemaster  :: T.Text
  , defaultchangebranch  :: T.Text
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
  deriving (Read, Eq)

deriving instance Show Version => Show Change

--------------------------------------------------------------------------------

data CommitRaw = CommitRaw
  { refs    :: Maybe [Ref]
  , rmessage :: Message
  } deriving (Show, Eq)

data Commit = Commit
  { tag     :: Maybe Tag
  , message :: Message
  } deriving (Show, Eq)

--------------------------------------------------------------------------------

data Version = Version
  { vmajor :: Int
  , vminor :: Int
  , vpatch :: Int
  , vcount :: Int
  } deriving (Read, Eq, Ord)

--------------------------------------------------------------------------------

data Result = Result
  { major      :: Int
  , minor      :: Int
  , patch      :: Int
  , count      :: Int
  , commithash :: T.Text
  , semver     :: T.Text
  } deriving (Generic)
