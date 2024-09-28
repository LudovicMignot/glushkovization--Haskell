import Debug.Trace
import NFA
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSize)
import Test.QuickCheck

genString :: Gen String
genString = sized $ \n -> do
  len <- choose (1, n)
  vectorOf len (elements ['a' .. 'e'])

genNFAAndStrings :: Int -> Int -> Gen (NFA Char Int, [String])
genNFAAndStrings maxlen nb = do
  nfa <- arbitrary
  strings <- vectorOf nb $ resize maxlen genString
  return (nfa, strings)

prop_equiv :: (Ord state1, Ord symbol, Ord state2) => NFA symbol state1 -> NFA symbol state2 -> [symbol] -> Property
prop_equiv nfa1 nfa2 s = property $ recognizes nfa1 s == recognizes nfa2 s

main :: IO ()
main = hspec $ do
  describe "makeHomogeneous" $ do
    modifyMaxSize (const 50) $
      it "makes an NFA homogeneous" $
        property $
          \nfa -> isHomogeneous $ makeHomogeneous (nfa :: NFA Char Int)
    modifyMaxSize (const 50) $
      it "preserves the language" $
        property $
          forAll (genNFAAndStrings 10 100) $
            \(nfa, strings) -> let nfa' = makeHomogeneous nfa in conjoin $ map (prop_equiv nfa nfa') strings

  describe "makeStandard" $ do
    modifyMaxSize (const 50) $
      it "makes an NFA standard" $
        property $
          \nfa -> isStandard $ makeStandard (nfa :: NFA Char Int)
    modifyMaxSize (const 50) $
      it "preserves the language" $
        property $
          forAll (genNFAAndStrings 10 100) $
            \(nfa, strings) -> let nfa' = makeStandard nfa in conjoin $ map (prop_equiv nfa nfa') strings

  describe "trim" $ do
    modifyMaxSize (const 50) $
      it "trims an NFA" $
        property $
          \nfa -> isTrim $ trim (nfa :: NFA Char Int)
    modifyMaxSize (const 50) $
      it "preserves the language" $
        property $
          forAll (genNFAAndStrings 10 100) $
            \(nfa, strings) -> let nfa' = trim nfa in conjoin $ map (prop_equiv nfa nfa') strings