import NFA
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "isHomogeneous" $ do
    it "makes an NFA homogeneous" $
      property $
        \nfa -> isHomogeneous $ makeHomogeneous (nfa :: NFA Char Int)