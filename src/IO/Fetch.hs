module IO.Fetch
  ( fetchCommitStrings
  , fetchCommitHash
  ) where

import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import           System.Process.Text.Lazy
import           Types

--------------------------------------------------------------------------------

fetchCommitStrings :: Branch -> IO (BranchMaster CommitString)

fetchCommitStrings (Branch "master") = do
  result <- readProcess "git" (["log", "master"] ++ logArgs)
  pure BranchMaster{master = CommitString result, branch = CommitString TL.empty}

fetchCommitStrings (Branch br) = do --TODO try and avoid duplicate call here (git command which does all at once?)
  resultOnlyBranch <- readProcess "git" (["log", "master.." ++ T.unpack br] ++ logArgs)
  resultTotal      <- readProcess "git" (["log", T.unpack br] ++ logArgs)
  let resultMaster = TL.take (fromIntegral $ TL.length resultTotal - TL.length resultOnlyBranch) resultTotal
  pure BranchMaster{master = CommitString resultMaster, branch = CommitString resultOnlyBranch}

--------------------------------------------------------------------------------

fetchCommitHash :: Branch -> IO CommitHash
fetchCommitHash (Branch br) = do
  result <- readProcess "git" ["rev-parse", T.unpack br]
  pure . CommitHash . TL.toStrict . TL.filter (/= '\n') $ result

--------------------------------------------------------------------------------

logArgs:: [String]
logArgs = ["-z", "--reverse", "--pretty=format:%D*%B", "--first-parent", "--decorate-refs='refs/tags/*'", "--"]

--------------------------------------------------------------------------------

readProcess :: FilePath -> [String] -> IO TL.Text
readProcess path args = do
  (_, stdout, _) <- readProcessWithExitCode path args ""
  pure stdout


