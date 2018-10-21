module IO.Fetch
  ( fetchCommitString
  ) where

import           System.Process
import           Data.Text (pack, unpack, empty)
import           Types

--------------------------------------------------------------------------------

fetchCommitString :: Branch -> IO (BranchMaster CommitString)

fetchCommitString (Branch "master") = do
  result <- readProcess "git" (["log", "master"] ++ sharedArgs) ""
  pure BranchMaster{master = CommitString . pack $ result, branch = CommitString empty}

fetchCommitString (Branch br) = do --TODO try and avoid duplicate call here (git command which does all at once?)
  resultOnlyBranch <- readProcess "git" (["log", "master.." ++ unpack br] ++ sharedArgs) ""
  resultTotal      <- readProcess "git" (["log", unpack br] ++ sharedArgs) ""
  let resultMaster = take (length resultTotal - length resultOnlyBranch) resultTotal
  pure BranchMaster{master = CommitString . pack $ resultMaster, branch = CommitString . pack $ resultOnlyBranch}

--------------------------------------------------------------------------------

sharedArgs:: [String]
sharedArgs= ["-z", "--reverse", "--pretty=format:%d|%B", "--first-parent", "--"]
