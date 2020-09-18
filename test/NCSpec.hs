module NCSpec where

import qualified Data.Text as Text
import Literal (Literal)
import qualified Literal as Lit
import NC
import Syntax (Index (..), Sign (..))
import Test.Hspec
import Test.QuickCheck

instance Arbitrary Index where
  arbitrary = Index <$> choose (0, 999)

instance Arbitrary Literal where
  arbitrary =
    frequency
      [ (4, Lit.Int <$> choose (0, 10 ^ (3 + digit) -1)),
        (4, Lit.Real <$> choose (0.0, 10 ^ (6 - digit) -1)),
        (1, Lit.Bool <$> arbitrary)
      ]

instance Arbitrary Sign where
  arbitrary = elements [Plus, Minus]

spec :: Spec
spec = do
  describe "definition" $ do
    it "should always be 72 chars long" $ do
      property $ \i def desc ->
        (length desc < 43)
          ==> Text.length (render $ pure $ Definiton i def (Text.pack desc)) === 72
