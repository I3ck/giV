module IO.Fetch
  ( fetchCommitString
  ) where

import           System.Process
import           Types

--------------------------------------------------------------------------------

---TODO some args are used 3 times here, avoid duplication

fetchCommitString :: Branch -> IO (BranchMaster CommitString)
fetchCommitString (Branch "master") = do
  result <- readProcess "git" ["log", "--reverse", "--pretty=format:%d|%s", "--first-parent", "master", "--"] ""
  pure $ BranchMaster{bMaster = CommitString result, bBranch = CommitString []}

fetchCommitString (Branch br) = do --TODO try and avoid duplicate call here (git command which does all at once?)
  resultOnlyBranch <- readProcess "git" ["log", "master.." ++ br, "--reverse", "--pretty=format:%d|%s", "--first-parent"] ""
  resultTotal      <- readProcess "git" ["log", "--reverse", "--pretty=format:%d|%s", "--first-parent", br, "--"] ""
  let resultMaster = take (length resultTotal - length resultOnlyBranch) resultTotal
  pure $ BranchMaster{bMaster = CommitString resultMaster, bBranch = CommitString resultOnlyBranch}
