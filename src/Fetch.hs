module Fetch
  ( fetchCommitString
  ) where

import           Types
import           System.Process

--------------------------------------------------------------------------------

fetchCommitString :: IO CommitString
fetchCommitString = gcommits

--------------------------------------------------------------------------------

git :: Params -> IO String
git ps = readProcess "git" ps ""

glog :: Params -> IO String
glog ps = git ("log" : ps)

glogfull :: Params -> IO String
glogfull ps = glog ("--full-history" : ps)

gcommits :: IO CommitString
gcommits = CommitString <$> glogfull ["--pretty=format:%d|%s"]
