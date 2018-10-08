{-# LANGUAGE RecordWildCards #-}

module Main where

import           Types
import           Fetch
import           Parse
import           Process
import           Utils

import           Data.Semigroup      ((<>))
import           Options.Applicative
import           System.Directory    (createDirectoryIfMissing,
                                      withCurrentDirectory)

--------------------------------------------------------------------------------

---TODO move below to Types?
data CliArgs = CliArgs
    { aGitDir    :: String
    }

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
    <> progDesc "gitV" --TODO more info
    <> header "gitV" --TODO more info
    )

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
      putStrLn "analysing..."

      --let processed = process now (aName args) raw
