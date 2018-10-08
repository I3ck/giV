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

data Change
  = Fix
  | Feature
  | Breaking
  | NoChange
  deriving (Show)

data Commit = Commit
  { cTag     :: Maybe Tag
  , cSubject :: Subject
  } deriving (Show)

data Raw = Raw
  { rCommits :: [Commit]
  } deriving (Show)

