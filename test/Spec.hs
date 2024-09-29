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

prop_equiv_rev :: (Ord state1, Ord symbol, Ord state2) => NFA symbol state1 -> NFA symbol state2 -> [symbol] -> Property
prop_equiv_rev nfa1 nfa2 s = property $ recognizes nfa1 s == recognizes nfa2 (Prelude.reverse s)

theGen :: Gen (NFA Char Int, [String])
theGen = genNFAAndStrings 10 100

main :: IO ()
main = hspec $ do
  describe "makeHomogeneous" $ do
    it "makes an NFA homogeneous" $
      property $
        forAll (resize 50 arbitrary) $
          \nfa -> isHomogeneous $ makeHomogeneous (nfa :: NFA Char Int)
    it "preserves the language" $
      property $
        forAll theGen $
          \(nfa, strings) -> let nfa' = makeHomogeneous nfa in conjoin $ map (prop_equiv nfa nfa') strings

  describe "makeStandard" $ do
    it "makes an NFA standard" $
      property $
        forAll (resize 50 arbitrary) $
          \nfa -> isStandard $ makeStandard (nfa :: NFA Char Int)
    it "preserves the language" $
      property $
        forAll theGen $
          \(nfa, strings) -> let nfa' = makeStandard nfa in conjoin $ map (prop_equiv nfa nfa') strings

  describe "trim" $ do
    it "trims an NFA" $
      property $
        forAll (resize 50 arbitrary) $
          \nfa -> isTrim $ trim (nfa :: NFA Char Int)
    it "preserves the language" $
      property $
        forAll theGen $
          \(nfa, strings) -> let nfa' = trim nfa in conjoin $ map (prop_equiv nfa nfa') strings

  describe "reverse" $ do
    it "preserves useful states" $
      property $
        \nfa -> let nfa' = NFA.reverse (nfa :: NFA Char Int) in getUsefulStates nfa == getUsefulStates nfa'
    it "reverses the language" $
      property $
        forAll theGen $
          \(nfa, strings) -> let nfa' = NFA.reverse nfa in conjoin $ map (prop_equiv_rev nfa nfa') strings
