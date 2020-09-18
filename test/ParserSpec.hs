module ParserSpec where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Text as Text
import qualified Literal as Lit
import Located
import qualified Parser
import Syntax
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (parse)

spec :: Spec
spec = do
  describe "expression" $ do
    let x `shouldParseTo` expr =
          parse Parser.expr "" (Text.pack x) `shouldParse` expr
    it "should parse simple expressions" $ do
      "1+1" `shouldParseTo` (let x = Lit (Lit.Int 1) in BinOp Add x x)
      "1.0-1.0" `shouldParseTo` (let x = Lit (Lit.Real 1.0) in BinOp Sub x x)
      "1*1" `shouldParseTo` (let x = Lit (Lit.Int 1) in BinOp Mul x x)
      "1.0/1.0" `shouldParseTo` (let x = Lit (Lit.Real 1.0) in BinOp Div x x)
      "-1" `shouldParseTo` (Neg (Lit (Lit.Int 1)))
      "-1.0" `shouldParseTo` (Neg (Lit (Lit.Real 1.0)))
      "2+2*2" `shouldParseTo` (let x = Lit (Lit.Int 2) in BinOp Add x (BinOp Mul x x))
      "(2+2)*2" `shouldParseTo` (let x = Lit (Lit.Int 2) in BinOp Mul (BinOp Add x x) x)
      "2-2/2" `shouldParseTo` (let x = Lit (Lit.Int 2) in BinOp Sub x (BinOp Div x x))
      "(2-2)/2" `shouldParseTo` (let x = Lit (Lit.Int 2) in BinOp Div (BinOp Sub x x) x)
      "(2-2)*(2+2)" `shouldParseTo` (let x = Lit (Lit.Int 2) in BinOp Mul (BinOp Sub x x) (BinOp Add x x))
      "2^3" `shouldParseTo` (Pow 3 (Lit (Lit.Int 2)))
    it "should parse function application" $ do
      "sin(90.0)" `shouldParseTo` (App "sin" [Lit (Lit.Real 90.0)])
      "f(1, 2.0/2)*2"
        `shouldParseTo` ( BinOp
                            Mul
                            ( App
                                "f"
                                [ Lit (Lit.Int 1),
                                  BinOp Div (Lit (Lit.Real 2.0)) (Lit (Lit.Int 2))
                                ]
                            )
                            (Lit (Lit.Int 2))
                        )
  describe "statement" $ do
    let x `shouldParseTo` stmt =
          ( fmap (\(At _ x') -> x') $
              parse Parser.statement "" (Text.pack x)
          )
            `shouldParse` stmt
    it "should parse assignment" $
      "test = 1.0" `shouldParseTo` Assign "test" (Lit (Lit.Real 1.0))
    it "should parse get" $ do
      "x <- X" `shouldParseTo` Get (pure "x") (pure "X")
      "x, y, z <- X, Y, Z" `shouldParseTo` Get ("x" :| ["y", "z"]) ("X" :| ["Y", "Z"])
    it "should parse set" $ do
      "X <- x" `shouldParseTo` Set (pure "X") (pure (Sym "x"))
      "X, Y, Z <- x, y, z" `shouldParseTo` Set ("X" :| ["Y", "Z"]) (Sym <$> "x" :| ["y", "z"])
    it "should parse codes" $
      "G00 X1.0" `shouldParseTo` Codes (Code "G" (Lit (Lit.Int 0)) :| [Code "X" (Lit (Lit.Real 1.0))])
    it "should parse label" $
      "label:" `shouldParseTo` Label "label"
