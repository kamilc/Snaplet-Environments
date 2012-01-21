module Snap.Snaplet.Environments.Instances where

import           Data.Configurator.Types
import qualified Data.UString as U
import qualified Data.Text as T

instance Configured U.UString where
  convert (String t) = Just $ U.u $ T.unpack t
  convert _ = Nothing