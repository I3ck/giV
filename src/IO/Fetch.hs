module IO.Fetch
  ( fetchCommitString
  ) where

import           System.Process
import           Types

--------------------------------------------------------------------------------

fetchCommitString :: Branch -> IO (BranchMaster CommitString)

fetchCommitString (Branch "master") = do
  result <- readProcess "git" (["log", "master"] ++ sharedArgs) ""
  pure BranchMaster{bMaster = CommitString result, bBranch = CommitString []}

fetchCommitString (Branch br) = do --TODO try and avoid duplicate call here (git command which does all at once?)
  resultOnlyBranch <- readProcess "git" (["log", "master.." ++ br] ++ sharedArgs) ""
  resultTotal      <- readProcess "git" (["log", br] ++ sharedArgs) ""
  let resultMaster = take (length resultTotal - length resultOnlyBranch) resultTotal
  pure BranchMaster{bMaster = CommitString resultMaster, bBranch = CommitString resultOnlyBranch}

--------------------------------------------------------------------------------

sharedArgs:: [String]
sharedArgs= ["--reverse", "--pretty=format:%d|%s", "--first-parent", "--"]
