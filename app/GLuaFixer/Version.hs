module GLuaFixer.Version where

import Data.Version (showVersion)
import qualified Paths_glualint (version)

version :: String
version = showVersion Paths_glualint.version
