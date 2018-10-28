module IO.Cfg
  ( loadCfg
  ) where

import           Instances            ()
import           Types

import           Control.Monad.Except (throwError)
import           Control.Monad.Trans  (liftIO)
import qualified Data.Text            as T
import qualified Data.Yaml            as Y

--------------------------------------------------------------------------------

loadCfg :: Args -> GiV CfgRaw
loadCfg Args{..} = do
  decode <- liftIO . Y.decodeFileEither . T.unpack $ aCfg
  case decode of
    Left e  -> throwError . YamlDecodeError . T.pack . show $ e
    Right x -> pure x

