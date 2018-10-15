module Main where

import           IO.Fetch
import           Parse
import           Process
import           Debugging
import           Types
import           Version
import           Utils
import           Instances ()

import           Control.Monad       (when)
import           Data.List           (find)
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
          changerules = fmap (\x -> ChangeRule (Regexp $ nameregexp x) (read $ defaultchange x)) . defaultchangerules $ cfg --TODO dont use read
          fallbackM   = read $ defaultchangemaster cfg --TODO dont use read
          fallbackB   = case find (\rule -> matches (nregexp rule) (branch args)) changerules of
                          Just r  -> dchange r
                          Nothing -> read $ defaultchangebranch cfg --TODO dont use read
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
          let changesB = process changergxs fallbackB commitsB
              changesM = process changergxs fallbackM commitsM
              v        = version $ BranchMaster changesB changesM
          when dbg $ print $ makeDebug cfg (BranchMaster fallbackB fallbackM) (BranchMaster commitsB commitsM) (BranchMaster changesB changesM) changerules
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
    <> progDesc "giV - Semantic versioning for Git repositories © Martin Buck"
    <> header "giV © Martin Buck"
    )
