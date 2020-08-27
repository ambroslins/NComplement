module Code where

import Code.G (G)
import qualified Code.G as G
import Compiler
import Parser

data Code
  = G G
  deriving (Eq, Show)

parser :: Parser Code
parser = G <$> G.parser

compile :: Code -> Compiler [Text]
compile (G g) = G.compile g
