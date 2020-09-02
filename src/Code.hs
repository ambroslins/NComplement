module Code where

import Code.G (G)
import qualified Code.G as G
import Generator
import Parser

data Code
  = G G
  deriving (Eq, Show)

parser :: Parser Code
parser = G <$> G.parser

generate :: Code -> Gen ()
generate (G g) = G.generate g
