module Types where

--------------------------------------------------------------------------------

type Params   = [String]
type UnixTime = Int
type Subject  = String
type Tag      = String

--------------------------------------------------------------------------------

newtype CommitString = CommitString
  { unCommitString :: String
  } deriving (Show)

--------------------------------------------------------------------------------

data CliArgs = CliArgs
  { repo :: String
  }

--------------------------------------------------------------------------------

data Change
  = NoChange
  | Fix
  | Feature
  | Breaking
  | SetTo Version
  deriving (Show)

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
  deriving (Show, Eq, Ord)
