module Main where

import           IO.Fetch
import           Parse
import           Process
import           Debugging
import           Types
import           Version
import           Instances ()

import           Control.Monad       (when)
import qualified Data.Yaml           as Y
import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Directory    (withCurrentDirectory)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- execParser opts
  eCfg <- loadCfg args
  case eCfg of
    Left e -> print e
    Right cfg -> do
      let gitdir      = repo args
          fallbackB   = read $ defaultchangebranch cfg
          fallbackM   = read $ defaultchangemaster cfg
          dbg         = verbose args
          changergxs  = ChangeRgxs 
                          (Regexp <$> majorregexp    cfg) 
                          (Regexp <$> minorregexp    cfg) 
                          (Regexp <$> patchregexp    cfg) 
                          (Regexp <$> nochangeregexp cfg)
                          (           tagversioning  cfg)
      when dbg $ putStrLn "Fetching..."
      cs <- withCurrentDirectory gitdir $ fetchCommitString $ Branch $ branch args
      when dbg $ putStrLn "Parsing..."
      case (parseCommitString $ bBranch cs, parseCommitString $ bMaster cs) of
        (Right commitsB, Right commitsM) -> do
          when dbg $ putStrLn "Processing..."
          let changesB = process changergxs fallbackB commitsB --TODO use specific fallback here
              changesM = process changergxs fallbackM commitsM --TODO use specific fallback here
              v        = version $ BranchMaster changesB changesM
          when dbg $ print $ makeDebug cfg (BranchMaster fallbackB fallbackM) (BranchMaster commitsB commitsM) (BranchMaster changesB changesM)
          print v
        (Left e, _) -> putStrLn $ "Error parsing commit data of branch: " ++ e
        (_, Left e) -> putStrLn $ "Error parsing commit data of master: " ++ e

--------------------------------------------------------------------------------

loadCfg :: CliArgs -> IO (Either Y.ParseException Cfg)
loadCfg CliArgs{..} = Y.decodeFileEither cfg

--------------------------------------------------------------------------------

args :: Parser CliArgs
args = CliArgs
  <$> strOption
    (  long "repo"
    <> short 'r'
    <> help "Path to the git repository that should be analyzed"
    <> metavar "STRING"
    )
  <*> strOption
    (  long "cfg"
    <> short 'c'
    <> help "Path to the configuration file"
    <> metavar "STRING"
    <> value "giVcfg.yaml"
    )
  <*> strOption
    (  long "branch"
    <> short 'b'
    <> help "The branch to analyze"
    <> metavar "STRING"
    <> value "master"
    )
  <*> switch
    (  long "verbose"
    <> short 'v'
    <> help "Display debugging information"
    <> showDefault
    )

opts :: ParserInfo CliArgs
opts = info (helper <*> args)
    (  fullDesc
    <> progDesc "giV - Semantic versioning for Git repositories"
    <> header "giV"
    )
