module Main where

import           IO.Fetch
import           Parse
import           Process
import           Types
import           Version
import           Instances ()

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Directory    (withCurrentDirectory)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- execParser opts
  let gitdir      = repo args
      fallback    = read $ defaultchange args
      changewords = ChangeWords (majorword args) (minorword args) (patchword args) (nochangeword args)
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

args :: Parser CliArgs
args = CliArgs
  <$> strOption
    (  long "repo"
    <> short 'r'
    <> help "Path to the git repository that should be analyzed"
    <> metavar "STRING"
    )
  <*> strOption
    ( long "defaultchange"
    <> short 'd'
    <> help "As what type of change a default commit shall be considered [NoChange, Fix, Feature, Breaking]"
    <> metavar "STRING"
    )
  <*> strOption
    ( long "majorword"
    <> help "Identifier that should be used for considering commits as breaking/changing major version"
    <> metavar "STRING"
    <> value "major"
    )
  <*> strOption
    ( long "minorword"
    <> help "Identifier that should be used for considering commits as feature/changing minor version"
    <> metavar "STRING"
    <> value "minor"
    )
  <*> strOption
    ( long "patchword"
    <> help "Identifier that should be used for considering commits as bug/changing patch version"
    <> metavar "STRING"
    <> value "patch"
    )
  <*> strOption
    ( long "nochangeword"
    <> help "Identifier that should be used for considering commits as no changes to version"
    <> metavar "STRING"
    <> value "nochange"
    )

opts :: ParserInfo CliArgs
opts = info (helper <*> args)
    (  fullDesc
    <> progDesc "giV - Semantic versioning for Git repositories"
    <> header "giV"
    )
