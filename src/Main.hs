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
import           Data.Text            (unpack)
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
  rargs <- liftIO . execParser $ opts
  args  <- liftEither . createArgs $ rargs
  rcfg  <- loadCfg args

  cfg <- liftEither . createCfg $ rcfg
  let gitdir      = aRepo args
      fallbacks   = createFallbacks cfg args (cDefaultChangerls cfg)
      dbg         = aVerbose args

  when dbg . liftIO $ putStrLn "Fetching..."
  cs <- liftIO . withCurrentDirectory (unpack gitdir) . fetchCommitString . Branch $ aBranch args

  when dbg . liftIO $ putStrLn "Parsing..."
  commitsB <- liftEither . parseCommitString $ branch cs
  commitsM <- liftEither . parseCommitString $ master cs
  when dbg . liftIO $ putStrLn "Processing..."
  let commits  = BranchMaster commitsB commitsM
      changes  = process cfg <$> fallbacks <*> commits
      v        = version changes
  when dbg . liftIO $ print . makeDebug cfg fallbacks commits $ changes
  case aOutput args of
    OutputVersion -> liftIO . print $ v

