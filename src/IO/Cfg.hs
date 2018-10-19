module IO.Cfg 
  ( loadCfg
  ) where

import           Types
import           Instances ()

import qualified Data.Yaml           as Y
import           Control.Monad.Except (throwError)
import           Control.Monad.Trans  (liftIO)

--------------------------------------------------------------------------------

loadCfg :: CliArgs -> GiV CfgRaw
loadCfg CliArgs{..} = do
  decode <- liftIO . Y.decodeFileEither $ aCfg
  case decode of
    Left e  -> throwError . YamlDecodeError . show $ e
    Right x -> pure x

