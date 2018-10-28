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

import           Control.Monad           (when)
import           Control.Monad.Except    (runExceptT, liftEither)
import           Control.Monad.Trans     (liftIO)
import qualified Data.Text               as T
import qualified Data.Yaml               as Y
import qualified Data.Aeson              as J
import           Options.Applicative
import qualified System.Directory        as SD
import qualified System.Exit             as SE
import qualified Data.String.Conversions as CV

--------------------------------------------------------------------------------

main :: IO ()
main = do
  result <- runExceptT giV
  case result of
    Right () -> pure ()
    Left e -> do
      print e
      SE.exitFailure

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
  cs   <- liftIO . SD.withCurrentDirectory (T.unpack gitdir) . fetchCommitStrings . Branch $ aBranch args
  hash <- liftIO . SD.withCurrentDirectory (T.unpack gitdir) . fetchCommitHash    . Branch $ aBranch args

  when dbg . liftIO $ putStrLn "Parsing..."
  commitsRB <- liftEither . parseCommitString $ branch cs
  commitsRM <- liftEither . parseCommitString $ master cs
  when dbg . liftIO $ putStrLn "Processing..."
  let commits = createCommits <$> BranchMaster commitsRB commitsRM
      changes = process cfg <$> fallbacks <*> commits
      v       = version (cStart cfg) changes
      sv      = semVerOf (aLabel args) v
      result  = createResult sv v hash
  when dbg . liftIO $ print . makeDebug cfg fallbacks commits $ changes
  case aOutput args of
    OutputVersion -> liftIO . putStrLn . asVersion $ result
    OutputYAML    -> liftIO . putStrLn . CV.cs . Y.encode $ result
    OutputJSON    -> liftIO . putStrLn . CV.cs . J.encode $ result

