import qualified Data.Set as Set
import NFA (NFA, recognizes, reverse)
import NFAAccessibility
  ( getAccessibleStates,
    getAccessibleStates',
    getUsefulStates,
    isTrim,
    trim,
  )
import NFABoolComb (symDiff)
import NFAHomogeneity (isHomogeneous, makeHomogeneous)
import NFAOrbit (kosarajuSet)
import NFAStandard (isStandard, makeStandard)
import Test.Hspec (describe, hspec, it, parallel)
import Test.QuickCheck
  ( Arbitrary (arbitrary),
    Gen,
    Property,
    Testable (property),
    choose,
    conjoin,
    elements,
    forAll,
    resize,
    sized,
    vectorOf,
  )

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
theGen = genNFAAndStrings 15 250

prop_symDiff :: (Ord state1, Ord symbol, Ord state2, Ord state3) => NFA symbol state2 -> NFA symbol state3 -> NFA symbol state1 -> [symbol] -> Bool
prop_symDiff nfa1 nfa2 nfa3 s = recognizes nfa3 s == (recognizes nfa1 s /= recognizes nfa2 s)

main :: IO ()
main = hspec $ parallel $ do
  describe "symDiff" $ parallel $ do
    it "computes the symmetric difference" $
      property $
        forAll (resize 20 arbitrary :: Gen (NFA Char Int)) $
          \nfa ->
            forAll (resize 20 theGen) $
              \(nfa', strings) ->
                let nfa'' = symDiff (nfa :: NFA Char Int) (nfa' :: NFA Char Int) in conjoin $ map (prop_symDiff nfa nfa' nfa'') strings

  describe "makeStandard" $ parallel $ do
    it "makes an NFA standard" $
      property $
        forAll (resize 35 arbitrary) $
          \nfa -> isStandard $ makeStandard (nfa :: NFA Char Int)
    it "preserves the language [empirically]" $
      property $
        forAll (resize 30 theGen) $
          \(nfa, strings) -> let nfa' = makeStandard nfa in conjoin $ map (prop_equiv nfa nfa') strings
    it "preserves the language" $
      property $
        forAll (resize 30 arbitrary) $
          \nfa -> Set.null $ getUsefulStates $ trim $ symDiff (nfa :: NFA Char Int) $ makeStandard nfa

  describe "trim" $ parallel $ do
    it "trims an NFA" $
      property $
        forAll (resize 35 arbitrary) $
          \nfa -> isTrim $ trim (nfa :: NFA Char Int)
    it "preserves the language [empirically]" $
      property $
        forAll (resize 30 theGen) $
          \(nfa, strings) -> let nfa' = trim nfa in conjoin $ map (prop_equiv nfa nfa') strings
    it "preserves the language" $
      property $
        forAll (resize 30 arbitrary) $
          \nfa -> Set.null $ getUsefulStates $ trim $ symDiff (nfa :: NFA Char Int) $ trim nfa

  describe "reverse" $ parallel $ do
    it "preserves the orbits" $
      property $
        \nfa -> let nfa' = NFA.reverse (nfa :: NFA Char Int) in kosarajuSet nfa == kosarajuSet nfa'
    it "preserves useful states" $
      property $
        \nfa -> let nfa' = NFA.reverse (nfa :: NFA Char Int) in getUsefulStates nfa == getUsefulStates nfa'
    it "reverses the language [empirically]" $
      property $
        forAll (resize 30 theGen) $
          \(nfa, strings) -> let nfa' = NFA.reverse nfa in conjoin $ map (prop_equiv_rev nfa nfa') strings
    it "preserves the language if applied twice" $
      property $
        forAll (resize 30 arbitrary) $
          \nfa -> Set.null $ getUsefulStates $ trim $ symDiff (nfa :: NFA Char Int) $ NFA.reverse $ NFA.reverse nfa

  describe "getAccessibleStates" $ parallel $ do
    it "is equal to getAccessibleStates'" $
      property $
        \nfa -> getAccessibleStates (nfa :: NFA Char Int) == getAccessibleStates' nfa

  describe "makeHomogeneous" $ parallel $ do
    it "makes an NFA homogeneous" $
      property $
        forAll (resize 35 arbitrary) $
          \nfa -> isHomogeneous $ makeHomogeneous (nfa :: NFA Char Int)
    it "preserves the language  [empirically]" $
      property $
        forAll (resize 30 theGen) $
          \(nfa, strings) -> let nfa' = makeHomogeneous nfa in conjoin $ map (prop_equiv nfa nfa') strings
    it "preserves the language" $
      property $
        forAll (resize 30 arbitrary) $
          \nfa -> Set.null $ getUsefulStates $ trim $ symDiff (nfa :: NFA Char Int) $ makeHomogeneous nfa
