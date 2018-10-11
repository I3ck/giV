module Fetch
  ( fetchCommitString
  ) where

import           System.Process
import           Types

--------------------------------------------------------------------------------

fetchCommitString :: IO CommitString
fetchCommitString = CommitString <$> readProcess "git" ["log", "--reverse", "--pretty=format:%d|%s", "--first-parent"] ""
