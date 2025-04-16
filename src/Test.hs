{-# LANGUAGE TypeApplications #-}

module Test where

import qualified Data.Foldable as F
import qualified Data.Map as Map
import Data.Set (fromList)
import qualified Data.Set as Set
import MonoEither (freeToA)
import NFA
  ( NFA (NFA, delta, delta_rev, final, initial),
    addTransitionInMap,
    generateNFA,
    generateNFASuchThat,
    getStates,
    reverse,
    reverseTransitionMap,
  )
import NFAAccessibility (getUsefulStates, isTrim, trim)
import NFABoolComb (symDiff)
import NFAComplete (complete)
import NFADotRepr (nfaToDot, toPngInImgDir)
import NFAHomogeneity
  ( generateHomogeneousNFA,
    isHomogeneous,
    makeHomogeneous,
  )
import NFAOrbit (externalIsolation, ingateIsolation, ingates, isIsolatedNFA, isStableNFA, isStronglyStableNFA, kosaraju, kosaraju1, kosarajuSet, orbitalIsolationViaSuccOutgates, orbitalSubstitution, outgates, strongStabilizationNFA)
import NFAStandard
  ( generateStandardNFA,
    isStandard,
    makeStandard,
  )
import System.IO (hFlush)
import System.IO.Extra (stdout)
import Test.QuickCheck (elements, generate)
import ToString (toString)

randomAut :: IO (NFA Char Int)
randomAut = generateNFA ['a' .. 'c'] [1 .. 7] 3 3 10

stdAut :: IO (NFA Char (Maybe Int))
stdAut = makeStandard <$> randomAut

a :: IO String
a = fmap nfaToDot randomAut

b :: IO FilePath
b = randomAut >>= toPngInImgDir "test"

run :: IO FilePath
run = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test" aut
  toPngInImgDir "test_std" $ makeStandard aut

runHomogeneous :: IO FilePath
runHomogeneous = do
  aut <- generateHomogeneousNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  toPngInImgDir "test_hom" aut

runStandard :: IO FilePath
runStandard = do
  aut <- generateStandardNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  toPngInImgDir "test_standard" aut

runStandardAndHomogeneous :: IO FilePath
runStandardAndHomogeneous = do
  aut <- generateNFASuchThat (\auto -> isStandard auto && isHomogeneous auto) ['a' .. 'c'] [1 .. 10 :: Int] 3 3 15
  toPngInImgDir "test_standard_hom" aut

runTestHomogeneous :: IO FilePath
runTestHomogeneous = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_hom_start" aut
  toPngInImgDir "test_hom_result" $ makeHomogeneous aut

runTestReversal :: IO FilePath
runTestReversal = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_rev_start" aut
  toPngInImgDir "test_rev_result" $ NFA.reverse aut

runUsefulTest :: IO ()
runUsefulTest = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_useful" aut
  let us = getUsefulStates aut
  putStrLn $ toString us

runTrimTest :: IO ()
runTrimTest = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_trim_start" aut
  _ <- toPngInImgDir "test_trim_result" $ trim aut
  let us = getUsefulStates aut
  putStrLn $ toString us

runKosa1 :: IO ()
runKosa1 = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 3 3 10
  _ <- toPngInImgDir "test_kosa1" aut
  let theList = kosaraju1 aut
  putStrLn $ toString theList

runKosa1' :: IO ()
runKosa1' = do
  let trans = [(1, 'a', 2), (4, 'a', 1), (3, 'a', 4), (5, 'a', 3), (4, 'a', 5)]
  let transitions = foldr (flip addTransitionInMap) Map.empty trans
  let aut = (NFA (Set.singleton 1) Set.empty transitions (reverseTransitionMap transitions) :: NFA Char Int)
  _ <- toPngInImgDir "test_kosa1'" aut
  let theList = kosaraju1 aut
  putStrLn $ toString theList

runKosa :: IO ()
runKosa = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 10 :: Int] 3 3 25
  _ <- toPngInImgDir "test_kosa" aut
  let theList = kosaraju aut
  print theList

runSymmDiff :: IO ()
runSymmDiff = do
  aut1 <- trim <$> generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 2 2 10
  aut2 <- trim <$> generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 2 2 10
  let aut3 = trim $ symDiff aut1 aut2
  _ <- toPngInImgDir "test_symdiff1" aut1
  _ <- toPngInImgDir "test_symdiff2" aut2
  _ <- toPngInImgDir "test_symdiff3" aut3
  print "Done"

runComplete :: IO ()
runComplete = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 7 :: Int] 2 2 10
  _ <- toPngInImgDir "test_complete1" aut
  _ <- toPngInImgDir "test_complete2" $ complete aut
  print "Done"

runExternalIsolation :: IO ()
runExternalIsolation = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 15 :: Int] 2 5 25
  _ <- toPngInImgDir "test_extIso1" aut
  let orbits = kosarajuSet aut
  let orbit = F.find (\o -> Set.size o >= 2) orbits
  case orbit of
    Nothing -> print "No size >=2 orbit"
    Just o -> do
      print o
      let m_g = Set.lookupMax $ outgates aut o
      case m_g of
        Nothing -> print "No gate"
        Just g -> do
          let aut' = externalIsolation aut o g
          print g
          _ <- toPngInImgDir "test_extIso2" aut'
          print "Done"

runingateIsolation :: IO ()
runingateIsolation = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 10 :: Int] 2 5 15
  _ <- toPngInImgDir "test_intIso1" aut
  let orbits = kosarajuSet aut
  let orbit = F.find (\o -> Set.size o >= 2) orbits
  case orbit of
    Nothing -> print "No size >=2 orbit"
    Just o -> do
      print o
      g <- generate $ elements $ Set.toList o
      print g
      let aut' = fst $ ingateIsolation aut o g
      _ <- toPngInImgDir "test_intIso2" aut'
      print "Done"

runOrbSubst :: IO ()
runOrbSubst = do
  let trans = foldr (flip addTransitionInMap) Map.empty [(1, 'a', 2), (1, 'b', 5), (2, 'a', 4), (3, 'a', 2), (4, 'a', 3), (4, 'c', 6), (4, 'a', 7), (5, 'a', 2), (5, 'c', 6), (6, 'a', 7), (7, 'c', 6)]
  let aut = (NFA (Set.singleton 1) (Set.fromList [4, 7]) trans (reverseTransitionMap trans) :: NFA Char Int)
  _ <- toPngInImgDir "test_subst" aut
  let o = Set.fromList [2, 3, 4]
  let o_i = ingates aut o
  let o_o = outgates aut o
  let trans' = foldr (flip addTransitionInMap) Map.empty [(1, 'a', 2), (1, 'b', 3), (2, 'c', 4), (3, 'a', 4), (4, 'b', 3)]
  let aut' = (NFA (Set.singleton 1) (Set.fromList [2, 3]) trans' (reverseTransitionMap trans') :: NFA Char Int)
  _ <- toPngInImgDir "test_subst'" aut'
  let aut'' = orbitalSubstitution aut o aut'
  _ <- toPngInImgDir "test_subst''" aut''
  print o_i
  print o_o
  putStrLn "Done"

runInOutGates :: IO ()
runInOutGates = do
  aut <- generateNFA ['a' .. 'c'] [1 .. 10 :: Int] 2 5 15
  let orbits = kosarajuSet aut
  let orbit = F.find (\o -> Set.size o >= 2) orbits
  case orbit of
    Nothing -> print "No size >=2 orbit"
    Just o -> do
      _ <- toPngInImgDir "test_gates" aut
      print o
      print $ outgates aut o
      print $ ingates aut o
      print "Done"

runTotalIsolation :: IO ()
runTotalIsolation = do
  aut <- trim <$> generateNFA ['a' .. 'e'] [1 .. 30 :: Int] 3 7 45
  let orbits_gates = Set.map (\o -> (ingates aut o, o, outgates aut o)) $ kosarajuSet aut
  putStr "Orbits and gates (aut): "
  putStrLn $ toString orbits_gates
  hFlush stdout
  let aut' = trim $ orbitalIsolationViaSuccOutgates aut
  putStr "Nb states of aut: "
  print $ F.length $ getStates aut
  putStr "Nb states of aut': "
  print $ F.length $ getStates aut'
  hFlush stdout

  let orbits_gates' = Set.map (\o -> (Set.map freeToA $ ingates aut' o, Set.map freeToA o, Set.map freeToA $ outgates aut' o)) $ kosarajuSet aut'
  putStr "Orbits and gates (aut'): "
  putStrLn $ toString orbits_gates'

  putStr "aut and aut' equivalent: "
  hFlush stdout
  print $ Set.null $ getUsefulStates $ symDiff aut aut'
  putStr "aut' isolated: "
  hFlush stdout
  print $ isIsolatedNFA aut'

  -- putStrLn "Printing aut"
  -- hFlush stdout
  -- _ <- toPngInImgDir "test_nfa_iso" aut

  -- putStrLn "Printing aut'"
  -- hFlush stdout
  -- _ <- toPngInImgDir "test_nfa_iso'" aut'

  putStrLn "Done"

runStabilization :: IO ()
runStabilization = do
  aut <- trim . makeStandard . makeHomogeneous <$> generateNFA ['a' .. 'b'] [1 .. 10 :: Int] 3 5 15
  let orbits_gates = Set.map (\o -> (ingates aut o, o, outgates aut o)) $ kosarajuSet aut
  putStr "Orbits and gates (aut): "
  putStrLn $ toString orbits_gates
  hFlush stdout
  let aut' = strongStabilizationNFA aut
  putStr "Nb states of aut: "
  print $ F.length $ getStates aut
  putStr "Nb states of aut': "
  print $ F.length $ getStates aut'
  hFlush stdout

  let orbits_gates' = Set.map (\o -> (Set.map freeToA $ ingates aut' o, Set.map freeToA o, Set.map freeToA $ outgates aut' o)) $ kosarajuSet aut'
  putStr "Orbits and gates (aut'): "
  putStrLn $ toString orbits_gates'

  putStr "aut and aut' equivalent: "
  hFlush stdout
  print $ Set.null $ getUsefulStates $ symDiff aut aut'
  putStr "aut' isolated [ok si False ou True]: "
  hFlush stdout
  print $ isIsolatedNFA aut'

  putStr "aut' stable: "
  hFlush stdout
  print $ isStableNFA aut'

  putStr "aut' strongly stable: "
  hFlush stdout
  print $ isStronglyStableNFA aut'

  putStr "aut' trim: "
  hFlush stdout
  print $ isTrim aut'

  putStrLn "Done"

runCounterExample :: IO ()
runCounterExample = do
  let trans = Map.fromList [(1 :: Int, Map.fromList [('a', fromList [4, 10])]), (4, Map.fromList [('b', fromList [1])]), (6, Map.fromList [('b', fromList [1])])]

  let aut_0 = NFA {initial = fromList [6], final = fromList [1, 6, 10], delta = trans, delta_rev = reverseTransitionMap trans}

  print $ isHomogeneous aut_0
  print $ isStandard aut_0
  print $ isTrim aut_0

  print $ isIsolatedNFA aut_0

  let aut = aut_0
  putStrLn "Printing aut"
  hFlush stdout
  _ <- toPngInImgDir "contre_ex" aut

  let orbits_gates' = Set.map (\o -> (ingates aut o, o, outgates aut o)) $ kosarajuSet aut
  putStr "Orbits and gates (aut'): "
  putStrLn $ toString orbits_gates'

  let aut' = orbitalIsolationViaSuccOutgates aut

  let (aut'2, new_states) = ingateIsolation aut (Set.fromList [1, 4]) 4

  print $ toString new_states

  putStrLn "Printing aut'2"
  hFlush stdout
  _ <- toPngInImgDir "contre_ex''" aut'2

  putStrLn "Printing aut'"
  hFlush stdout
  _ <- toPngInImgDir "contre_ex'" aut'

  print $ F.length $ getStates aut'
  print $ F.length $ getStates $ trim aut'

  print $ isTrim aut'
  print $ isStronglyStableNFA $ trim aut'

  print "done"