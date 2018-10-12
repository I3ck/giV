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

data CliArgs = CliArgs
  { repo    :: String
  , cfg     :: String
  , verbose :: Bool
  }

--------------------------------------------------------------------------------

data Cfg = Cfg
  { defaultchange :: String
  , majorword     :: String
  , minorword     :: String
  , patchword     :: String
  , nochangeword  :: String
  } deriving (Generic)

--------------------------------------------------------------------------------

data ChangeWords = ChangeWords
  { majorw    :: String
  , minorw    :: String
  , patchw    :: String
  , nochangew :: String
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
