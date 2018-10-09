module Main where

import           Types
import           Fetch
import           Parse
import           Process
import           Version

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Directory    (withCurrentDirectory)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- execParser opts
  let gitdir = aGitDir args
  putStrLn "fetching commit data..."
  cs <- withCurrentDirectory gitdir fetchCommitString
  putStrLn "parsing commit data..."
  case parseCommitString cs of
    Left e    -> putStrLn $ "Error parsing commit data: " ++ e
    Right raw -> do
      putStrLn "processing..."
      let changes = process raw
      --putStrLn . show $ changes
      let v = version changes
      putStrLn . show $ v

--------------------------------------------------------------------------------

args :: Parser CliArgs
args = CliArgs
  <$> strOption
      (  long "gitdir"
      <> short 'g'
      <> help "Path to the git directory that shall be used"
      <> metavar "STRING"
      )

opts :: ParserInfo CliArgs
opts = info (helper <*> args)
    (  fullDesc
    <> progDesc "giV" --TODO more info
    <> header "giV" --TODO more info
    )
