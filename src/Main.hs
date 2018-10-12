module Main where

import           IO.Fetch
import           Parse
import           Process
import           Types
import           Version
import           Instances ()

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
          changewords = ChangeWords (majorword cfg) (minorword cfg) (patchword cfg) (nochangeword cfg)
      putStrLn "Fetching..."
      cs <- withCurrentDirectory gitdir fetchCommitString
      putStrLn "Parsing..."
      case parseCommitString cs of
        Left e    -> putStrLn $ "Error parsing commit data: " ++ e
        Right raw -> do
          putStrLn "Processing..."
          let changes = process changewords fallback raw
          let v = version changes
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

opts :: ParserInfo CliArgs
opts = info (helper <*> args)
    (  fullDesc
    <> progDesc "giV - Semantic versioning for Git repositories"
    <> header "giV"
    )
