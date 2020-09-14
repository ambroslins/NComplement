module ParserSpec where

import qualified Data.Text as Text
import qualified Literal as Lit
import qualified Parser
import Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "expression" $ do
    let x `shouldParseTo` expr =
          it ("should parse " <> show x) $
            parse Parser.expr "" (Text.pack x) `shouldParse` expr
    "1+1" `shouldParseTo` (let x = Lit (Lit.Int 1) in Add x x)
    "1+1*1" `shouldParseTo` (let x = Lit (Lit.Int 1) in Add x (Mul x x))
    "(1+1)*1" `shouldParseTo` (let x = Lit (Lit.Int 1) in Mul (Add x x) x)
    "var+2" `shouldParseTo` Add (Sym "var") (Lit (Lit.Int 2))
    "fun(1, 2)" `shouldParseTo` App "fun" [(Lit (Lit.Int 1)), (Lit (Lit.Int 2))]
