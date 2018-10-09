module Fetch
  ( fetchCommitString
  ) where

import           Types
import           System.Process

--------------------------------------------------------------------------------

fetchCommitString :: IO CommitString
fetchCommitString = CommitString <$> readProcess "git" ["log", "--pretty=format:%d|%s", "--full-history"] ""
