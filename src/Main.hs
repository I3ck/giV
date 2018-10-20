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
import           Result
import           Instances ()

import           Control.Monad        (when)
import           Control.Monad.Except (runExceptT, liftEither)
import           Control.Monad.Trans  (liftIO)
import           Data.Text            (unpack)
import qualified Data.Yaml  as Y
import qualified Data.Aeson as J
import           Options.Applicative
import           System.Directory     (withCurrentDirectory)
import           System.Exit          (exitFailure)
import qualified Data.String.Conversions as CV

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
      result   = createResult v
  when dbg . liftIO $ print . makeDebug cfg fallbacks commits $ changes
  case aOutput args of
    OutputVersion -> liftIO . putStrLn . asVersion $ result
    OutputYAML    -> liftIO . putStrLn . CV.cs . Y.encode $ result
    OutputJSON    -> liftIO . putStrLn . CV.cs . J.encode $ result

