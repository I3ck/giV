module Args
  ( opts
  ) where

import           Types

import           Options.Applicative

--------------------------------------------------------------------------------

opts :: ParserInfo ArgsRaw
opts = info (helper <*> args)
    (  fullDesc
    <> progDesc "giV 0.3.0 - Semantic versioning for Git repositories © Martin Buck"
    <> header "giV 0.3.0 © Martin Buck"
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
    (  long "outputformat"
    <> short 'o'
    <> help "The output format [OutputVersion | OutputYAML | OutputJSON]"
    <> metavar "STRING"
    <> value "OutputVersion" 
    )
  <*> switch
    (  long "verbose"
    <> short 'v'
    <> help "Output debugging information"
    <> showDefault
    )
