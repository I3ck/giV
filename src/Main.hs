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
          fallback    = read $ defaultchange cfg
          dbg         = verbose args
          changergxs  = ChangeRgxs 
                          (Regexp <$> majorregexp    cfg) 
                          (Regexp <$> minorregexp    cfg) 
                          (Regexp <$> patchregexp    cfg) 
                          (Regexp <$> nochangeregexp cfg)
                          (           tagversioning  cfg)
      when dbg $ putStrLn "Fetching..."
      cs <- withCurrentDirectory gitdir fetchCommitString
      when dbg $ putStrLn "Parsing..."
      case parseCommitString cs of
        Left e    -> putStrLn $ "Error parsing commit data: " ++ e
        Right commits -> do
          when dbg $ putStrLn "Processing..."
          let changes = process changergxs fallback commits
              v       = version changes
          when dbg $ print $ makeDebug cfg fallback commits changes
          print v

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
