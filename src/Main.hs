module Main where

import           Fetch
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
  let gitdir = repo args
  putStrLn "Fetching..."
  cs <- withCurrentDirectory gitdir fetchCommitString
  putStrLn "Parsing..."
  case parseCommitString cs of
    Left e    -> putStrLn $ "Error parsing commit data: " ++ e
    Right raw -> do
      putStrLn "Processing..."
      let changes = process raw
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

opts :: ParserInfo CliArgs
opts = info (helper <*> args)
    (  fullDesc
    <> progDesc "giV - Semantic versioning for Git repositories"
    <> header "giV"
    )
