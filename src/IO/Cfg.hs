module IO.Cfg 
  ( loadCfg
  ) where

import           Types
import           Instances ()

import qualified Data.Yaml           as Y

--------------------------------------------------------------------------------

loadCfg :: CliArgs -> IO (Either Y.ParseException Cfg)
loadCfg CliArgs{..} = Y.decodeFileEither cfg

