module Main where

import           Args
import           Create
import           IO.Fetch
import           IO.Cfg
import           Parse
import           Process
import           Debugging
import           Types
import           Version
import           Instances ()

import           Control.Monad       (when)
import           Options.Applicative
import           System.Directory    (withCurrentDirectory)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- execParser opts
  erCfg <- loadCfg args

  case erCfg of
    Left e    -> print e
    Right rcfg -> do

      let cfg         = createCfg rcfg
          gitdir      = repo args
          fallbacks   = createFallbacks cfg args (defaultchangerls cfg)
          dbg         = verbose args

      when dbg $ putStrLn "Fetching..."
      cs <- withCurrentDirectory gitdir $ fetchCommitString $ Branch $ branch args

      when dbg $ putStrLn "Parsing..."
      case (parseCommitString $ bBranch cs, parseCommitString $ bMaster cs) of
        (Right commitsB, Right commitsM) -> do
          when dbg $ putStrLn "Processing..."
          let commits  = BranchMaster commitsB commitsM
              changes  = process cfg <$> fallbacks <*> commits
              v        = version changes
          when dbg $ print $ makeDebug cfg fallbacks commits changes
          print v
        (Left e, _) -> putStrLn $ "Error parsing commit data of branch: " ++ e
        (_, Left e) -> putStrLn $ "Error parsing commit data of master: " ++ e

