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

import           Control.Monad        (when)
import           Control.Monad.Except (runExceptT, liftEither)
import           Control.Monad.Trans  (liftIO)
import           Options.Applicative
import           System.Directory     (withCurrentDirectory)
import           System.Exit          (exitFailure)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  result <- runExceptT giV
  case result of
    Right () -> pure ()
    Left e -> do
      print e
      exitFailure

--------------------------------------------------------------------------------

giV :: GiV ()
giV = do
  args  <- liftIO . execParser $ opts
  rcfg <- loadCfg args

  cfg <- liftEither . createCfg $ rcfg
  let gitdir      = repo args
      fallbacks   = createFallbacks cfg args (defaultchangerls cfg)
      dbg         = verbose args

  when dbg . liftIO $ putStrLn "Fetching..."
  cs <- liftIO . withCurrentDirectory gitdir . fetchCommitString . Branch $ branch args

  when dbg . liftIO $ putStrLn "Parsing..."
  commitsB <- liftEither . parseCommitString $ bBranch cs
  commitsM <- liftEither . parseCommitString $ bMaster cs
  when dbg . liftIO $ putStrLn "Processing..."
  let commits  = BranchMaster commitsB commitsM
      changes  = process cfg <$> fallbacks <*> commits
      v        = version changes
  when dbg . liftIO $ print . makeDebug cfg fallbacks commits $ changes
  liftIO . print $ v

