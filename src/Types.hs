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

newtype CommitString = CommitString
  { unCommitString :: String
  } deriving (Show)

--------------------------------------------------------------------------------

newtype Regexp = Regexp
 { unRegexp :: String
 }

--------------------------------------------------------------------------------

data CliArgs = CliArgs
  { repo    :: String
  , cfg     :: String
  , verbose :: Bool
  }

--------------------------------------------------------------------------------

data Cfg = Cfg
  { defaultchange  :: String
  , majorregexp    :: String
  , minorregexp    :: String
  , patchregexp    :: String
  , nochangeregexp :: String
  } deriving (Generic)

--------------------------------------------------------------------------------

data DebugInfo = DebugInfo
  { dDefault  :: Change
  , dMajor    :: String
  , dMinor    :: String
  , dPatch    :: String
  , dNoChange :: String
  , dLines    :: [DebugLine]
  }

--------------------------------------------------------------------------------

data DebugLine = DebugLine
  { dVersion :: Version
  , dChange  :: Change
  , dCommit  :: Commit
  }

--------------------------------------------------------------------------------

data ChangeRgxs = ChangeRgxs
  { majorrgx    :: Regexp
  , minorrgx    :: Regexp
  , patchrgx    :: Regexp
  , nochangergx :: Regexp
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

data Raw = Raw
  { commits :: [Commit]
  } deriving (Show)

--------------------------------------------------------------------------------

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  }
  deriving (Read, Eq, Ord)
