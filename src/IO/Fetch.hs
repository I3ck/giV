module IO.Fetch
  ( fetchCommitString
  ) where

import           Types
import           System.Process.Text.Lazy
import           Data.Text
import qualified Data.Text.Lazy as TL

--------------------------------------------------------------------------------

fetchCommitString :: Branch -> IO (BranchMaster CommitString)

fetchCommitString (Branch "master") = do
  result <- readProcess "git" (["log", "master"] ++ sharedArgs)
  pure BranchMaster{master = CommitString result, branch = CommitString TL.empty}

fetchCommitString (Branch br) = do --TODO try and avoid duplicate call here (git command which does all at once?)
  resultOnlyBranch <- readProcess "git" (["log", "master.." ++ unpack br] ++ sharedArgs)
  resultTotal      <- readProcess "git" (["log", unpack br] ++ sharedArgs)
  let resultMaster = TL.take (fromIntegral $ TL.length resultTotal - TL.length resultOnlyBranch) resultTotal
  pure BranchMaster{master = CommitString resultMaster, branch = CommitString resultOnlyBranch}

--------------------------------------------------------------------------------

sharedArgs:: [String]
sharedArgs = ["-z", "--reverse", "--pretty=format:%d|%B", "--first-parent", "--"]

--------------------------------------------------------------------------------

readProcess :: FilePath -> [String] -> IO TL.Text
readProcess path args = do
  (_, stdout, _) <- readProcessWithExitCode path args ""
  pure stdout


