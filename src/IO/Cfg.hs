module IO.Cfg 
  ( loadCfg
  ) where

import           Types
import           Instances ()

import qualified Data.Yaml           as Y
import           Data.Text            (pack, unpack)
import           Control.Monad.Except (throwError)
import           Control.Monad.Trans  (liftIO)

--------------------------------------------------------------------------------

loadCfg :: Args -> GiV CfgRaw
loadCfg Args{..} = do
  decode <- liftIO . Y.decodeFileEither . unpack $ aCfg
  case decode of
    Left e  -> throwError . YamlDecodeError . pack . show $ e
    Right x -> pure x

