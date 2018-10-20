module Args
  ( opts
  ) where

import           Types

import           Options.Applicative

--------------------------------------------------------------------------------

opts :: ParserInfo ArgsRaw
opts = info (helper <*> args)
    (  fullDesc
    <> progDesc "giV - Semantic versioning for Git repositories © Martin Buck"
    <> header "giV © Martin Buck"
    )

--------------------------------------------------------------------------------

args :: Parser ArgsRaw
args = ArgsRaw
  <$> strOption
    (  long "repo"
    <> short 'r'
    <> help "Path to the git repository that should be versioned"
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
    <> help "The branch that should be versioned"
    <> metavar "STRING"
    <> value "master"
    )
  <*> strOption
    (  long "outputmode"
    <> short 'o'
    <> help "The output mode [OutputVersion]"
    <> metavar "STRING"
    <> value "OutputVersion" 
    )
  <*> switch
    (  long "verbose"
    <> short 'v'
    <> help "Output debugging information"
    <> showDefault
    )
